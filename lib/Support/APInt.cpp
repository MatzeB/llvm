//===-- APInt.cpp - Implement APInt class ---------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements a class to represent arbitrary precision integer
// constant values and provide a variety of arithmetic operations on them.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/APInt.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <limits>
using namespace llvm;

#define DEBUG_TYPE "apint"

/// A utility function for allocating memory, checking for allocation failures,
/// and ensuring the contents are zeroed.
inline static uint64_t* getClearedMemory(unsigned numWords) {
  uint64_t * result = new uint64_t[numWords];
  assert(result && "APInt memory allocation fails!");
  memset(result, 0, numWords * sizeof(uint64_t));
  return result;
}

/// A utility function for allocating memory and checking for allocation
/// failure.  The content is not zeroed.
inline static uint64_t* getMemory(unsigned numWords) {
  uint64_t * result = new uint64_t[numWords];
  assert(result && "APInt memory allocation fails!");
  return result;
}

/// A utility function that converts a character to a digit.
inline static unsigned getDigit(char cdigit, uint8_t radix) {
  unsigned r;

  if (radix == 16 || radix == 36) {
    r = cdigit - '0';
    if (r <= 9)
      return r;

    r = cdigit - 'A';
    if (r <= radix - 11U)
      return r + 10;

    r = cdigit - 'a';
    if (r <= radix - 11U)
      return r + 10;
    
    radix = 10;
  }

  r = cdigit - '0';
  if (r < radix)
    return r;

  return -1U;
}

void APIntImpl::initSlowCase(uint64_t Val, bool IsSigned) {
  if (needsCleanup())
    OutOfLineStorage = getClearedMemory(getNumWords());
  words().front() = Val;
  if (IsSigned && int64_t(Val) < 0)
    for (uint64_t &Word : make_range(std::next(words().begin()), words().end()))
      Word = -1ULL;
}

void APIntImpl::initSlowCase(APIntRef ThatRef) {
  if (needsCleanup())
    OutOfLineStorage = getMemory(getNumWords());
  std::copy(ThatRef.words().begin(), ThatRef.words().end(), words().begin());
}

void APIntImpl::initFromArray(ArrayRef<uint64_t> BigVal) {
  assert(BitWidth && "Bitwidth too small");
  assert(BigVal.data() && "Null pointer detected!");
  if (isSingleWord()) {
    *InlineStorage = BigVal.front();
  } else {
    // Get memory, cleared to 0
    if (needsCleanup())
      OutOfLineStorage = getClearedMemory(getNumWords());
    else
      clearAllBits();
    // Calculate the number of words to copy
    unsigned NumWords = std::min<unsigned>(BigVal.size(), getNumWords());
    // Copy the words from bigVal to pVal
    std::copy(BigVal.begin(), BigVal.begin() + NumWords, words().begin());
  }
  // Make sure unused high bits are cleared
  clearUnusedBits();
}

void APIntImpl::assignSlowCase(APIntRef RHS) {
  if (getNumWords() != RHS.getNumWords()) {
    if (needsCleanup())
      delete [] OutOfLineStorage;
    if (wouldNeedCleanup(RHS.BitWidth))
      OutOfLineStorage = getMemory(RHS.getNumWords());
  }
  BitWidth = RHS.BitWidth;
  std::copy(RHS.words().begin(), RHS.words().end(), words().begin());
}

/// This method 'profiles' an APInt for use with FoldingSet.
void APIntRef::Profile(FoldingSetNodeID &ID) const {
  ID.AddInteger(BitWidth);
  for (uint64_t Word : words())
    ID.AddInteger(Word);
}

/// This function adds a single "digit" integer, y, to the multiple
/// "digit" integer array,  x[]. x[] is modified to reflect the addition and
/// 1 is returned if there is a carry out, otherwise 0 is returned.
/// @returns the carry of the addition.
static bool add_1(uint64_t dest[], uint64_t x[], unsigned len, uint64_t y) {
  for (unsigned i = 0; i < len; ++i) {
    dest[i] = y + x[i];
    if (dest[i] < y)
      y = 1; // Carry one to next digit.
    else {
      y = 0; // No need to carry so exit early
      break;
    }
  }
  return y;
}

/// @brief Prefix increment operator. Increments the APInt by one.
void APIntImpl::increment() {
  if (isSingleWord())
    ++InlineStorage[0];
  else
    add_1(words().begin(), words().begin(), getNumWords(), 1);
  clearUnusedBits();
}

/// This function subtracts a single "digit" (64-bit word), y, from
/// the multi-digit integer array, x[], propagating the borrowed 1 value until
/// no further borrowing is neeeded or it runs out of "digits" in x.  The result
/// is 1 if "borrowing" exhausted the digits in x, or 0 if x was not exhausted.
/// In other words, if y > x then this function returns 1, otherwise 0.
/// @returns the borrow out of the subtraction
static bool sub_1(uint64_t x[], unsigned len, uint64_t y) {
  for (unsigned i = 0; i < len; ++i) {
    uint64_t X = x[i];
    x[i] -= y;
    if (y > X)
      y = 1;  // We have to "borrow 1" from next "digit"
    else {
      y = 0;  // No need to borrow
      break;  // Remaining digits are unchanged so exit early
    }
  }
  return bool(y);
}

/// @brief Prefix decrement operator. Decrements the APInt by one.
void APIntImpl::decrement() {
  if (isSingleWord())
    --InlineStorage[0];
  else
    sub_1(OutOfLineStorage, getNumWords(), 1);
  return clearUnusedBits();
}

/// This function adds the integer array x to the integer array Y and
/// places the result in dest.
/// @returns the carry out from the addition
/// @brief General addition of 64-bit integer arrays
static bool add(uint64_t *dest, const uint64_t *x, const uint64_t *y,
                unsigned len) {
  bool carry = false;
  for (unsigned i = 0; i< len; ++i) {
    uint64_t limit = std::min(x[i],y[i]); // must come first in case dest == x
    dest[i] = x[i] + y[i] + carry;
    carry = dest[i] < limit || (carry && dest[i] == limit);
  }
  return carry;
}

/// Adds the RHS APint to this APInt.
/// @brief Addition assignment operator.
void APIntImpl::add(APIntRef RHS) {
  assert(BitWidth == RHS.BitWidth && "Bit widths must be the same");
  if (isSingleWord())
    InlineStorage[0] += RHS.words().front();
  else
    ::add(words().begin(), words().begin(), RHS.words().begin(), getNumWords());
  return clearUnusedBits();
}

void APIntImpl::add(uint64_t RHS) {
  if (isSingleWord())
    InlineStorage[0] += RHS;
  else
    add_1(words().begin(), words().begin(), getNumWords(), RHS);
  return clearUnusedBits();
}

/// Subtracts the integer array y from the integer array x
/// @returns returns the borrow out.
/// @brief Generalized subtraction of 64-bit integer arrays.
static bool sub(uint64_t *dest, const uint64_t *x, const uint64_t *y,
                unsigned len) {
  bool borrow = false;
  for (unsigned i = 0; i < len; ++i) {
    uint64_t x_tmp = borrow ? x[i] - 1 : x[i];
    borrow = y[i] > x_tmp || (borrow && x[i] == 0);
    dest[i] = x_tmp - y[i];
  }
  return borrow;
}

/// Subtracts the RHS APInt from this APInt
/// @returns this, after subtraction
/// @brief Subtraction assignment operator.
void APIntImpl::subtract(APIntRef RHS) {
  assert(BitWidth == RHS.BitWidth && "Bit widths must be the same");
  if (isSingleWord())
    InlineStorage[0] -= RHS.words().front();
  else
    sub(words().begin(), words().begin(), RHS.words().begin(), getNumWords());
  clearUnusedBits();
}

void APIntImpl::subtract(uint64_t RHS) {
  if (isSingleWord())
    InlineStorage[0] -= RHS;
  else
    sub_1(words().begin(), getNumWords(), RHS);
  clearUnusedBits();
}

/// Multiplies an integer array, x, by a uint64_t integer and places the result
/// into dest.
/// @returns the carry out of the multiplication.
/// @brief Multiply a multi-digit APInt by a single digit (64-bit) integer.
static uint64_t mul_1(uint64_t dest[], uint64_t x[], unsigned len, uint64_t y) {
  // Split y into high 32-bit part (hy)  and low 32-bit part (ly)
  uint64_t ly = y & 0xffffffffULL, hy = y >> 32;
  uint64_t carry = 0;

  // For each digit of x.
  for (unsigned i = 0; i < len; ++i) {
    // Split x into high and low words
    uint64_t lx = x[i] & 0xffffffffULL;
    uint64_t hx = x[i] >> 32;
    // hasCarry - A flag to indicate if there is a carry to the next digit.
    // hasCarry == 0, no carry
    // hasCarry == 1, has carry
    // hasCarry == 2, no carry and the calculation result == 0.
    uint8_t hasCarry = 0;
    dest[i] = carry + lx * ly;
    // Determine if the add above introduces carry.
    hasCarry = (dest[i] < carry) ? 1 : 0;
    carry = hx * ly + (dest[i] >> 32) + (hasCarry ? (1ULL << 32) : 0);
    // The upper limit of carry can be (2^32 - 1)(2^32 - 1) +
    // (2^32 - 1) + 2^32 = 2^64.
    hasCarry = (!carry && hasCarry) ? 1 : (!carry ? 2 : 0);

    carry += (lx * hy) & 0xffffffffULL;
    dest[i] = (carry << 32) | (dest[i] & 0xffffffffULL);
    carry = (((!carry && hasCarry != 2) || hasCarry == 1) ? (1ULL << 32) : 0) +
            (carry >> 32) + ((lx * hy) >> 32) + hx * hy;
  }
  return carry;
}

/// Multiplies integer array x by integer array y and stores the result into
/// the integer array dest. Note that dest's size must be >= xlen + ylen.
/// @brief Generalized multiplicate of integer arrays.
static void mul(uint64_t dest[], uint64_t x[], unsigned xlen, const uint64_t y[],
                unsigned ylen) {
  dest[xlen] = mul_1(dest, x, xlen, y[0]);
  for (unsigned i = 1; i < ylen; ++i) {
    uint64_t ly = y[i] & 0xffffffffULL, hy = y[i] >> 32;
    uint64_t carry = 0, lx = 0, hx = 0;
    for (unsigned j = 0; j < xlen; ++j) {
      lx = x[j] & 0xffffffffULL;
      hx = x[j] >> 32;
      // hasCarry - A flag to indicate if has carry.
      // hasCarry == 0, no carry
      // hasCarry == 1, has carry
      // hasCarry == 2, no carry and the calculation result == 0.
      uint8_t hasCarry = 0;
      uint64_t resul = carry + lx * ly;
      hasCarry = (resul < carry) ? 1 : 0;
      carry = (hasCarry ? (1ULL << 32) : 0) + hx * ly + (resul >> 32);
      hasCarry = (!carry && hasCarry) ? 1 : (!carry ? 2 : 0);

      carry += (lx * hy) & 0xffffffffULL;
      resul = (carry << 32) | (resul & 0xffffffffULL);
      dest[i+j] += resul;
      carry = (((!carry && hasCarry != 2) || hasCarry == 1) ? (1ULL << 32) : 0)+
              (carry >> 32) + (dest[i+j] < resul ? 1 : 0) +
              ((lx * hy) >> 32) + hx * hy;
    }
    dest[i+xlen] = carry;
  }
}

void APIntImpl::multiply(APIntRef RHS) {
  assert(BitWidth == RHS.BitWidth && "Bit widths must be the same");
  if (isSingleWord()) {
    InlineStorage[0] *= RHS.words().front();
    clearUnusedBits();
    return;
  }

  // Get some bit facts about LHS and check for zero
  unsigned lhsBits = APIntRef(*this).getActiveBits();
  unsigned lhsWords = !lhsBits ? 0 : APIntRef::whichWord(lhsBits - 1) + 1;
  if (!lhsWords)
    // 0 * X ===> 0
    return;

  // Get some bit facts about RHS and check for zero
  unsigned rhsBits = APIntRef(RHS).getActiveBits();
  unsigned rhsWords = !rhsBits ? 0 : APIntRef::whichWord(rhsBits - 1) + 1;
  if (!rhsWords) {
    // X * 0 ===> 0
    clearAllBits();
    return;
  }

  // Allocate space for the result
  uint64_t *Dest;
  unsigned DestWords = rhsWords + lhsWords;
  uint64_t StackDest[16];
  uint64_t *HeapDest = nullptr;
  if (DestWords > array_lengthof(StackDest))
    Dest = HeapDest = getMemory(DestWords);
  else
    Dest = StackDest;

  // Perform the long multiply
  mul(Dest, &words().front(), lhsWords, &RHS.words().front(), rhsWords);

  // Copy result back into *this
  clearAllBits();
  unsigned wordsToCopy = std::min(DestWords, getNumWords());
  memcpy(&words().front(), Dest, wordsToCopy * APINT_WORD_SIZE);
  clearUnusedBits();

  if (HeapDest) {
    // delete dest array and return
    delete[] HeapDest;
  }
}

void APIntImpl::and_op(APIntRef RHS) {
  assert(BitWidth == RHS.BitWidth && "Bit widths must be the same");
  if (isSingleWord()) {
    InlineStorage[0] &= RHS.words().front();
    return;
  }
  unsigned numWords = getNumWords();
  for (unsigned i = 0; i < numWords; ++i)
    words()[i] &= RHS.words()[i];
}

void APIntImpl::or_op(APIntRef RHS) {
  assert(BitWidth == RHS.BitWidth && "Bit widths must be the same");
  if (isSingleWord()) {
    InlineStorage[0] |= RHS.words().front();
    return;
  }
  unsigned numWords = getNumWords();
  for (unsigned i = 0; i < numWords; ++i)
    words()[i] |= RHS.words()[i];
}

void APIntImpl::xor_op(APIntRef RHS) {
  assert(BitWidth == RHS.BitWidth && "Bit widths must be the same");
  if (isSingleWord()) {
    InlineStorage[0] ^= RHS.words().front();
    clearUnusedBits();
    return;
  }
  unsigned numWords = getNumWords();
  for (unsigned i = 0; i < numWords; ++i)
    words()[i] ^= RHS.words()[i];
  clearUnusedBits();
}

bool APIntRef::EqualSlowCase(APIntRef RHS) const {
  return std::equal(words().begin(), words().end(), RHS.words().begin());
}

bool APIntRef::EqualSlowCase(uint64_t Val) const {
  unsigned n = getActiveBits();
  if (n <= APINT_BITS_PER_WORD)
    return words()[0] == Val;
  else
    return false;
}

bool APIntRef::ult(APIntRef RHS) const {
  assert(BitWidth == RHS.BitWidth && "Bit widths must be same for comparison");
  if (isSingleWord())
    return words()[0] < RHS.words()[0];

  // Get active bit length of both operands
  unsigned n1 = getActiveBits();
  unsigned n2 = RHS.getActiveBits();

  // If magnitude of LHS is less than RHS, return true.
  if (n1 < n2)
    return true;

  // If magnitude of RHS is greather than LHS, return false.
  if (n2 < n1)
    return false;

  // If they bot fit in a word, just compare the low order word
  if (n1 <= APINT_BITS_PER_WORD && n2 <= APINT_BITS_PER_WORD)
    return words()[0] < RHS.words()[0];

  // Otherwise, compare all words
  unsigned topWord = whichWord(std::max(n1, n2) - 1);
  for (int i = topWord; i >= 0; --i) {
    if (words()[i] > RHS.words()[i])
      return false;
    if (words()[i] < RHS.words()[i])
      return true;
  }
  return false;
}

bool APIntRef::slt(APIntRef RHS) const {
  assert(BitWidth == RHS.BitWidth && "Bit widths must be same for comparison");
  if (isSingleWord()) {
    int64_t lhsSext = SignExtend64(words()[0], BitWidth);
    int64_t rhsSext = SignExtend64(RHS.words()[0], BitWidth);
    return lhsSext < rhsSext;
  }

  bool lhsNeg = isNegative();
  bool rhsNeg = RHS.isNegative();

  // If the sign bits don't match, then (LHS < RHS) if LHS is negative
  if (lhsNeg != rhsNeg)
    return lhsNeg;

  // Otherwise we can just use an unsigned comparision, because even negative
  // numbers compare correctly this way if both have the same signed-ness.
  return ult(RHS);
}


unsigned APIntRef::getBitsNeeded(StringRef str, uint8_t radix) {
  assert(!str.empty() && "Invalid string length");
  assert((radix == 10 || radix == 8 || radix == 16 || radix == 2 || 
          radix == 36) &&
         "Radix should be 2, 8, 10, 16, or 36!");

  size_t slen = str.size();

  // Each computation below needs to know if it's negative.
  StringRef::iterator p = str.begin();
  unsigned isNegative = *p == '-';
  if (*p == '-' || *p == '+') {
    p++;
    slen--;
    assert(slen && "String is only a sign, needs a value.");
  }

  // For radixes of power-of-two values, the bits required is accurately and
  // easily computed
  if (radix == 2)
    return slen + isNegative;
  if (radix == 8)
    return slen * 3 + isNegative;
  if (radix == 16)
    return slen * 4 + isNegative;

  // FIXME: base 36
  
  // This is grossly inefficient but accurate. We could probably do something
  // with a computation of roughly slen*64/20 and then adjust by the value of
  // the first few digits. But, I'm not sure how accurate that could be.

  // Compute a sufficient number of bits that is always large enough but might
  // be too large. This avoids the assertion in the constructor. This
  // calculation doesn't work appropriately for the numbers 0-9, so just use 4
  // bits in that case.
  unsigned sufficient 
    = radix == 10? (slen == 1 ? 4 : slen * 64/18)
                 : (slen == 1 ? 7 : slen * 16/3);

  // Convert to the actual binary value.
  APInt tmp(sufficient, StringRef(p, slen), radix);

  // Compute how many bits are required. If the log is infinite, assume we need
  // just bit.
  unsigned log = tmp.logBase2();
  if (log == (unsigned)-1) {
    return isNegative + 1;
  } else {
    return isNegative + log + 1;
  }
}

hash_code llvm::hash_value(APIntRef Arg) {
  if (Arg.isSingleWord())
    return hash_combine(Arg.words()[0]);

  return hash_value(Arg.words());
}

unsigned APIntRef::countLeadingZerosSlowCase() const {
  unsigned Count = 0;
  for (uint64_t V : reverse(words())) {
    if (V == 0)
      Count += APINT_BITS_PER_WORD;
    else {
      Count += llvm::countLeadingZeros(V);
      break;
    }
  }
  // Adjust for unused bits in the most significant word (they are zero).
  unsigned Mod = BitWidth % APINT_BITS_PER_WORD;
  Count -= Mod > 0 ? APINT_BITS_PER_WORD - Mod : 0;
  return Count;
}

unsigned APIntRef::countLeadingOnes() const {
  if (isSingleWord())
    return llvm::countLeadingOnes(words()[0]
                                  << (APINT_BITS_PER_WORD - BitWidth));

  unsigned highWordBits = BitWidth % APINT_BITS_PER_WORD;
  unsigned shift;
  if (!highWordBits) {
    highWordBits = APINT_BITS_PER_WORD;
    shift = 0;
  } else {
    shift = APINT_BITS_PER_WORD - highWordBits;
  }
  int i = getNumWords() - 1;
  unsigned Count = llvm::countLeadingOnes(words()[i] << shift);
  if (Count == highWordBits) {
    for (i--; i >= 0; --i) {
      if (words()[i] == -1ULL)
        Count += APINT_BITS_PER_WORD;
      else {
        Count += llvm::countLeadingOnes(words()[i]);
        break;
      }
    }
  }
  return Count;
}

unsigned APIntRef::countTrailingZeros() const {
  if (isSingleWord())
    return std::min(unsigned(llvm::countTrailingZeros(words()[0])), BitWidth);
  unsigned Count = 0;
  unsigned i = 0;
  for (; i < getNumWords() && words()[i] == 0; ++i)
    Count += APINT_BITS_PER_WORD;
  if (i < getNumWords())
    Count += llvm::countTrailingZeros(words()[i]);
  return std::min(Count, BitWidth);
}

unsigned APIntRef::countTrailingOnesSlowCase() const {
  unsigned Count = 0;
  unsigned i = 0;
  for (; i < getNumWords() && words()[i] == -1ULL; ++i)
    Count += APINT_BITS_PER_WORD;
  if (i < getNumWords())
    Count += llvm::countTrailingOnes(words()[i]);
  return std::min(Count, BitWidth);
}

unsigned APIntRef::countPopulationSlowCase() const {
  unsigned Count = 0;
  for (uint64_t Word : words())
    Count += llvm::countPopulation(Word);
  return Count;
}

/// Perform a logical right-shift from Src to Dst, which must be equal or
/// non-overlapping, of Words words, by Shift, which must be less than 64.
static void lshrNear(uint64_t *Dst, uint64_t *Src, unsigned Words,
                     unsigned Shift) {
  uint64_t Carry = 0;
  for (int I = Words - 1; I >= 0; --I) {
    uint64_t Tmp = Src[I];
    Dst[I] = (Tmp >> Shift) | Carry;
    Carry = Tmp << (64 - Shift);
  }
}

void APIntImpl::byteSwap() {
  assert(BitWidth >= 16 && BitWidth % 16 == 0 && "Cannot byteswap!");
  if (BitWidth == 16) {
    InlineStorage[0] = ByteSwap_16(uint16_t(InlineStorage[0]));
    return;
  }
  if (BitWidth == 32) {
    InlineStorage[0] = ByteSwap_32(unsigned(InlineStorage[0]));
    return;
  }
  if (BitWidth == 48) {
    unsigned Tmp1 = unsigned(InlineStorage[0] >> 16);
    Tmp1 = ByteSwap_32(Tmp1);
    uint16_t Tmp2 = uint16_t(InlineStorage[0]);
    Tmp2 = ByteSwap_16(Tmp2);
    InlineStorage[0] = (uint64_t(Tmp2) << 32) | Tmp1;
    return;
  }
  if (BitWidth == 64) {
    InlineStorage[0] = ByteSwap_64(InlineStorage[0]);
    return;
  }

  APInt Result(getNumWords() * APINT_BITS_PER_WORD, 0);
  std::reverse(words().begin(), words().end());
  for (uint64_t &Word : words())
    Word = ByteSwap_64(Word);

  uint64_t NumBits = getNumWords() * APINT_BITS_PER_WORD;
  if (NumBits != BitWidth)
    lshrNear(words().begin(), words().begin(), getNumWords(),
             NumBits - BitWidth);
}

void APIntImpl::reverseBits() {
  switch (BitWidth) {
  case 64: {
    InlineStorage[0] = llvm::reverseBits<uint64_t>(InlineStorage[0]);
    return;
  }
  case 32: {
    InlineStorage[0] = llvm::reverseBits<uint32_t>(InlineStorage[0]);
    return;
  }
  case 16: {
    InlineStorage[0] = llvm::reverseBits<uint16_t>(InlineStorage[0]);
    return;
  }
  case 8: {
    InlineStorage[0] = llvm::reverseBits<uint8_t>(InlineStorage[0]);
    return;
  }
  default:
    break;
  }

  unsigned FirstIter = 0;
  unsigned LastIter = BitWidth;
  while ((FirstIter != LastIter) && (FirstIter != --LastIter)) {
    unsigned FirstBitPos = FirstIter++;
    bool FirstTrue = APIntRef(*this)[FirstBitPos];
    bool LastTrue = APIntRef(*this)[LastIter];
    if (LastTrue)
      setBit(FirstBitPos);
    else
      clearBit(FirstBitPos);
    if (FirstTrue)
      setBit(LastIter);
    else
      clearBit(LastIter);
  }
}

APInt llvm::APIntOps::RoundDoubleToAPInt(double Double, unsigned width) {
  union {
    double D;
    uint64_t I;
  } T;
  T.D = Double;

  // Get the sign bit from the highest order bit
  bool isNeg = T.I >> 63;

  // Get the 11-bit exponent and adjust for the 1023 bit bias
  int64_t exp = ((T.I >> 52) & 0x7ff) - 1023;

  // If the exponent is negative, the value is < 0 so just return 0.
  if (exp < 0)
    return APInt(width, 0u);

  // Extract the mantissa by clearing the top 12 bits (sign + exponent).
  uint64_t mantissa = (T.I & (~0ULL >> 12)) | 1ULL << 52;

  // If the exponent doesn't shift all bits out of the mantissa
  if (exp < 52)
    return isNeg ? -APInt(width, mantissa >> (52 - exp)) :
                    APInt(width, mantissa >> (52 - exp));

  // If the client didn't provide enough bits for us to shift the mantissa into
  // then the result is undefined, just return 0
  if (width <= exp - 52)
    return APInt(width, 0);

  // Otherwise, we have to shift the mantissa bits up to the right location
  APInt Tmp(width, mantissa);
  Tmp = Tmp.shl((unsigned)exp - 52);
  return isNeg ? -Tmp : Tmp;
}

/// This function converts this APInt to a double.
/// The layout for double is as following (IEEE Standard 754):
///  --------------------------------------
/// |  Sign    Exponent    Fraction    Bias |
/// |-------------------------------------- |
/// |  1[63]   11[62-52]   52[51-00]   1023 |
///  --------------------------------------
double APIntRef::roundToDouble(bool isSigned) const {
  // Handle the simple case where the value is contained in one uint64_t.
  if (isSingleWord() || getActiveBits() <= APINT_BITS_PER_WORD) {
    if (isSigned) {
      int64_t sext = SignExtend64(words()[0], BitWidth);
      return double(sext);
    } else
      return double(words()[0]);
  }

  // Determine if the value is negative.
  bool isNeg = isSigned ? (*this)[BitWidth-1] : false;

  // Construct the absolute value if we're negative.
  APInt Tmp(*this);
  if (isNeg)
    Tmp = -Tmp;

  // Figure out how many bits we're using.
  unsigned n = Tmp.getActiveBits();

  // The exponent (without bias normalization) is just the number of bits
  // we are using. Note that the sign bit is gone since we constructed the
  // absolute value.
  uint64_t exp = n;

  // Return infinity for exponent overflow
  if (exp > 1023) {
    if (!isSigned || !isNeg)
      return std::numeric_limits<double>::infinity();
    else
      return -std::numeric_limits<double>::infinity();
  }
  exp += 1023; // Increment for 1023 bias

  // Number of bits in mantissa is 52. To obtain the mantissa value, we must
  // extract the high 52 bits from the correct words in pVal.
  uint64_t mantissa;
  unsigned hiWord = whichWord(n-1);
  if (hiWord == 0) {
    mantissa = Tmp.pVal[0];
    if (n > 52)
      mantissa >>= n - 52; // shift down, we want the top 52 bits.
  } else {
    assert(hiWord > 0 && "huh?");
    uint64_t hibits = Tmp.pVal[hiWord] << (52 - n % APINT_BITS_PER_WORD);
    uint64_t lobits = Tmp.pVal[hiWord-1] >> (11 + n % APINT_BITS_PER_WORD);
    mantissa = hibits | lobits;
  }

  // The leading bit of mantissa is implicit, so get rid of it.
  uint64_t sign = isNeg ? (1ULL << (APINT_BITS_PER_WORD - 1)) : 0;
  union {
    double D;
    uint64_t I;
  } T;
  T.I = sign | (exp << 52) | mantissa;
  return T.D;
}

// Truncate to new width.
void APIntImpl::trunc(unsigned width) {
  assert(width < BitWidth && "Invalid APInt Truncate request");
  assert(width && "Can't truncate to 0 bits");

  // Copy the words if they were previously heap allocated but the result fits
  // in the inline storage.
  if (needsCleanup() && width <= getNumInlineBits()) {
    uint64_t *ToCleanup = OutOfLineStorage;
    unsigned NewNumWords =
        alignTo<APINT_BITS_PER_WORD>(width) / APINT_BITS_PER_WORD;
    std::copy(words().begin(), words().begin() + NewNumWords, InlineStorage);
    delete[] ToCleanup;
  }

  // Perform the truncation.
  BitWidth = width;
  clearUnusedBits();
}

// Sign extend to a new width.
void APIntImpl::sext(unsigned width) {
  assert(width > BitWidth && "Invalid APInt SignExtend request");

  if (width <= APINT_BITS_PER_WORD) {
    uint64_t val = InlineStorage[0] << (APINT_BITS_PER_WORD - BitWidth);
    val = (int64_t)val >> (width - BitWidth);
    InlineStorage[0] = val >> (APINT_BITS_PER_WORD - width);
    BitWidth = width;
    return;
  }

  unsigned NewNumWords =
      alignTo<APINT_BITS_PER_WORD>(width) / APINT_BITS_PER_WORD;
  uint64_t *Dest;
  if (NewNumWords > NumInlineWords)
    Dest = getMemory(NewNumWords);
  else
    Dest = InlineStorage;

  // Copy full words.
  unsigned i;
  uint64_t word = 0;
  for (i = 0; i != BitWidth / APINT_BITS_PER_WORD; i++) {
    word = words()[i];
    Dest[i] = word;
  }

  // Read and sign-extend any partial word.
  unsigned bits = (0 - BitWidth) % APINT_BITS_PER_WORD;
  if (bits != 0)
    word = (int64_t)words()[i] << bits >> bits;
  else
    word = (int64_t)word >> (APINT_BITS_PER_WORD - 1);

  // Write remaining full words.
  for (; i != width / APINT_BITS_PER_WORD; i++) {
    Dest[i] = word;
    word = (int64_t)word >> (APINT_BITS_PER_WORD - 1);
  }

  // Write any partial word.
  bits = (0 - width) % APINT_BITS_PER_WORD;
  if (bits != 0)
    Dest[i] = word << bits >> bits;

  if (needsCleanup())
    delete[] OutOfLineStorage;
  BitWidth = width;
  if (Dest != InlineStorage)
    OutOfLineStorage = Dest;
}

//  Zero extend to a new width.
void APIntImpl::zext(unsigned width) {
  assert(width > BitWidth && "Invalid APInt ZeroExtend request");

  unsigned NewNumWords =
      alignTo<APINT_BITS_PER_WORD>(width) / APINT_BITS_PER_WORD;
  if (NewNumWords == getNumWords()) {
    BitWidth = width;
    return;
  }

  uint64_t *Dest;
  if (NewNumWords > NumInlineWords) {
    Dest = getMemory(NewNumWords);
    // Copy words.
    std::copy(words().begin(), words().begin() + getNumWords(), Dest);
  } else {
    Dest = InlineStorage;
  }

  // Zero remaining words.
  unsigned i = getNumWords() + 1;
  memset(&Dest[i], 0, (NewNumWords - i) * APINT_WORD_SIZE);

  if (needsCleanup())
    delete[] OutOfLineStorage;
  BitWidth = width;
  if (NewNumWords > NumInlineWords)
    OutOfLineStorage = Dest;
}

/// Arithmetic right-shift this APInt by shiftAmt.
/// @brief Arithmetic right-shift function.
void APIntImpl::ashr(unsigned shiftAmt) {
  assert(shiftAmt <= BitWidth && "Invalid shift amount");
  // Handle a degenerate case
  if (shiftAmt == 0)
    return;

  // Handle single word shifts with built-in ashr
  if (isSingleWord()) {
    if (shiftAmt == BitWidth)
      InlineStorage[0] = 0; // undefined
    else
      InlineStorage[0] = SignExtend64(InlineStorage[0], BitWidth) >> shiftAmt;
    return;
  }

  // If all the bits were shifted out, the result is, technically, undefined.
  // We return -1 if it was negative, 0 otherwise. We check this early to avoid
  // issues in the algorithm below.
  if (shiftAmt == BitWidth) {
    if (isNegative())
      initSlowCase(-1ULL, /*IsSigned=*/true);
    else
      initSlowCase(0, /*IsSigned=*/false);
    return;
  }

  // Compute some values needed by the following shift algorithms
  unsigned wordShift = shiftAmt % APINT_BITS_PER_WORD; // bits to shift per word
  unsigned offset = shiftAmt / APINT_BITS_PER_WORD; // word offset for shift
  unsigned breakWord = getNumWords() - 1 - offset; // last word affected
  unsigned bitsInWord =
      APIntRef::whichBit(BitWidth); // how many bits in last word?
  if (bitsInWord == 0)
    bitsInWord = APINT_BITS_PER_WORD;

  // If we are shifting whole words, just move whole words
  if (wordShift == 0) {
    // Move the words containing significant bits
    for (unsigned i = 0; i <= breakWord; ++i)
      words()[i] = words()[i+offset]; // move whole word

    // Adjust the top significant word for sign bit fill, if negative
    if (isNegative())
      if (bitsInWord < APINT_BITS_PER_WORD)
        words()[breakWord] |= ~0ULL << bitsInWord; // set high bits
  } else {
    // Shift the low order words
    for (unsigned i = 0; i < breakWord; ++i) {
      // This combines the shifted corresponding word with the low bits from
      // the next word (shifted into this word's high bits).
      words()[i] =
          (words()[i + offset] >> wordShift) |
          (words()[i + offset + 1] << (APINT_BITS_PER_WORD - wordShift));
    }

    // Shift the break word. In this case there are no bits from the next word
    // to include in this word.
    words()[breakWord] = words()[breakWord + offset] >> wordShift;

    // Deal with sign extension in the break word, and possibly the word before
    // it.
    if (isNegative()) {
      if (wordShift > bitsInWord) {
        if (breakWord > 0)
          words()[breakWord-1] |=
            ~0ULL << (APINT_BITS_PER_WORD - (wordShift - bitsInWord));
        words()[breakWord] |= ~0ULL;
      } else
        words()[breakWord] |= (~0ULL << (bitsInWord - wordShift));
    }
  }

  // Remaining words are 0 or -1, just assign them.
  uint64_t fillValue = (isNegative() ? -1ULL : 0);
  for (unsigned i = breakWord+1; i < getNumWords(); ++i)
    words()[i] = fillValue;
  clearUnusedBits();
}

/// Logical right-shift this APInt by shiftAmt.
/// @brief Logical right-shift function.
void APIntImpl::lshr(unsigned shiftAmt) {
  // If all the bits were shifted out, the result is 0. This avoids issues
  // with shifting by the size of the integer type, which produces undefined
  // results. We define these "undefined results" to always be 0.
  if (shiftAmt >= BitWidth) {
    clearAllBits();
    return;
  }

  if (isSingleWord()) {
    InlineStorage[0] >>= shiftAmt;
    return;
  }

  // If none of the bits are shifted out, the result is *this. This avoids
  // issues with shifting by the size of the integer type, which produces
  // undefined results in the code below. This is also an optimization.
  if (shiftAmt == 0)
    return;

  // If we are shifting less than a word, compute the shift with a simple carry
  if (shiftAmt < APINT_BITS_PER_WORD) {
    lshrNear(words().begin(), words().end(), getNumWords(), shiftAmt);
    clearUnusedBits();
    return;
  }

  // Compute some values needed by the remaining shift algorithms
  unsigned wordShift = shiftAmt % APINT_BITS_PER_WORD;
  unsigned offset = shiftAmt / APINT_BITS_PER_WORD;

  // If we are shifting whole words, just move whole words
  if (wordShift == 0) {
    for (unsigned i = 0; i < getNumWords() - offset; ++i)
      words()[i] = words()[i+offset];
    for (unsigned i = getNumWords()-offset; i < getNumWords(); i++)
      words()[i] = 0;
    clearUnusedBits();
    return;
  }

  // Shift the low order words
  unsigned breakWord = getNumWords() - offset -1;
  for (unsigned i = 0; i < breakWord; ++i)
    words()[i] = (words()[i + offset] >> wordShift) |
                 (words()[i + offset + 1] << (APINT_BITS_PER_WORD - wordShift));
  // Shift the break word.
  words()[breakWord] = words()[breakWord + offset] >> wordShift;

  // Remaining words are 0
  for (unsigned i = breakWord+1; i < getNumWords(); ++i)
    words()[i] = 0;
  clearUnusedBits();
}

void APIntImpl::shl(unsigned shiftAmt) {
  assert(shiftAmt <= BitWidth && "Invalid shift amount");
  if (isSingleWord()) {
    if (shiftAmt >= BitWidth)
      clearAllBits();
    else
      InlineStorage[0] <<= shiftAmt;
    return;
  }

  // If all the bits were shifted out, the result is 0. This avoids issues
  // with shifting by the size of the integer type, which produces undefined
  // results. We define these "undefined results" to always be 0.
  if (shiftAmt == BitWidth) {
    clearAllBits();
    return;
  }

  // If none of the bits are shifted out, the result is *this. This avoids a
  // lshr by the words size in the loop below which can produce incorrect
  // results. It also avoids the expensive computation below for a common case.
  if (shiftAmt == 0)
    return;

  // If we are shifting less than a word, do it the easy way
  if (shiftAmt < APINT_BITS_PER_WORD) {
    uint64_t carry = 0;
    for (unsigned i = 0; i < getNumWords(); i++) {
      uint64_t Shifted = words()[i] << shiftAmt | carry;
      carry = words()[i] >> (APINT_BITS_PER_WORD - shiftAmt);
      words()[i] = Shifted;
    }
    clearUnusedBits();
    return;
  }

  // Compute some values needed by the remaining shift algorithms
  unsigned wordShift = shiftAmt % APINT_BITS_PER_WORD;
  unsigned offset = shiftAmt / APINT_BITS_PER_WORD;

  // If we are shifting whole words, just move whole words
  if (wordShift == 0) {
    for (unsigned i = offset; i < getNumWords(); i++)
      words()[i] = words()[i - offset];
    for (unsigned i = 0; i < offset; i++)
      words()[i] = 0;
    clearUnusedBits();
    return;
  }

  // Copy whole words from this to Result.
  for (unsigned i = getNumWords() - 1; i > offset; --i)
    words()[i] = words()[i - offset] << wordShift |
                 words()[i - offset - 1] >> (APINT_BITS_PER_WORD - wordShift);
  words()[offset] = words()[0] << wordShift;
  for (unsigned i = 0; i < offset; ++i)
    words()[i] = 0;

  clearUnusedBits();
}

/// Implementation of Knuth's Algorithm D (Division of nonnegative integers)
/// from "Art of Computer Programming, Volume 2", section 4.3.1, p. 272. The
/// variables here have the same names as in the algorithm. Comments explain
/// the algorithm and any deviation from it.
static void KnuthDiv(unsigned *u, unsigned *v, unsigned *q, unsigned* r,
                     unsigned m, unsigned n) {
  assert(u && "Must provide dividend");
  assert(v && "Must provide divisor");
  assert(q && "Must provide quotient");
  assert(u != v && u != q && v != q && "Must use different memory");
  assert(n>1 && "n must be > 1");

  // b denotes the base of the number system. In our case b is 2^32.
  LLVM_CONSTEXPR uint64_t b = uint64_t(1) << 32;

  DEBUG(dbgs() << "KnuthDiv: m=" << m << " n=" << n << '\n');
  DEBUG(dbgs() << "KnuthDiv: original:");
  DEBUG(for (int i = m+n; i >=0; i--) dbgs() << " " << u[i]);
  DEBUG(dbgs() << " by");
  DEBUG(for (int i = n; i >0; i--) dbgs() << " " << v[i-1]);
  DEBUG(dbgs() << '\n');
  // D1. [Normalize.] Set d = b / (v[n-1] + 1) and multiply all the digits of
  // u and v by d. Note that we have taken Knuth's advice here to use a power
  // of 2 value for d such that d * v[n-1] >= b/2 (b is the base). A power of
  // 2 allows us to shift instead of multiply and it is easy to determine the
  // shift amount from the leading zeros.  We are basically normalizing the u
  // and v so that its high bits are shifted to the top of v's range without
  // overflow. Note that this can require an extra word in u so that u must
  // be of length m+n+1.
  unsigned shift = countLeadingZeros(v[n-1]);
  unsigned v_carry = 0;
  unsigned u_carry = 0;
  if (shift) {
    for (unsigned i = 0; i < m+n; ++i) {
      unsigned u_tmp = u[i] >> (32 - shift);
      u[i] = (u[i] << shift) | u_carry;
      u_carry = u_tmp;
    }
    for (unsigned i = 0; i < n; ++i) {
      unsigned v_tmp = v[i] >> (32 - shift);
      v[i] = (v[i] << shift) | v_carry;
      v_carry = v_tmp;
    }
  }
  u[m+n] = u_carry;

  DEBUG(dbgs() << "KnuthDiv:   normal:");
  DEBUG(for (int i = m+n; i >=0; i--) dbgs() << " " << u[i]);
  DEBUG(dbgs() << " by");
  DEBUG(for (int i = n; i >0; i--) dbgs() << " " << v[i-1]);
  DEBUG(dbgs() << '\n');

  // D2. [Initialize j.]  Set j to m. This is the loop counter over the places.
  int j = m;
  do {
    DEBUG(dbgs() << "KnuthDiv: quotient digit #" << j << '\n');
    // D3. [Calculate q'.].
    //     Set qp = (u[j+n]*b + u[j+n-1]) / v[n-1]. (qp=qprime=q')
    //     Set rp = (u[j+n]*b + u[j+n-1]) % v[n-1]. (rp=rprime=r')
    // Now test if qp == b or qp*v[n-2] > b*rp + u[j+n-2]; if so, decrease
    // qp by 1, inrease rp by v[n-1], and repeat this test if rp < b. The test
    // on v[n-2] determines at high speed most of the cases in which the trial
    // value qp is one too large, and it eliminates all cases where qp is two
    // too large.
    uint64_t dividend = ((uint64_t(u[j+n]) << 32) + u[j+n-1]);
    DEBUG(dbgs() << "KnuthDiv: dividend == " << dividend << '\n');
    uint64_t qp = dividend / v[n-1];
    uint64_t rp = dividend % v[n-1];
    if (qp == b || qp*v[n-2] > b*rp + u[j+n-2]) {
      qp--;
      rp += v[n-1];
      if (rp < b && (qp == b || qp*v[n-2] > b*rp + u[j+n-2]))
        qp--;
    }
    DEBUG(dbgs() << "KnuthDiv: qp == " << qp << ", rp == " << rp << '\n');

    // D4. [Multiply and subtract.] Replace (u[j+n]u[j+n-1]...u[j]) with
    // (u[j+n]u[j+n-1]..u[j]) - qp * (v[n-1]...v[1]v[0]). This computation
    // consists of a simple multiplication by a one-place number, combined with
    // a subtraction.
    // The digits (u[j+n]...u[j]) should be kept positive; if the result of
    // this step is actually negative, (u[j+n]...u[j]) should be left as the
    // true value plus b**(n+1), namely as the b's complement of
    // the true value, and a "borrow" to the left should be remembered.
    int64_t borrow = 0;
    for (unsigned i = 0; i < n; ++i) {
      uint64_t p = uint64_t(qp) * uint64_t(v[i]);
      int64_t subres = int64_t(u[j+i]) - borrow - (unsigned)p;
      u[j+i] = (unsigned)subres;
      borrow = (p >> 32) - (subres >> 32);
      DEBUG(dbgs() << "KnuthDiv: u[j+i] = " << u[j+i]
                   << ", borrow = " << borrow << '\n');
    }
    bool isNeg = u[j+n] < borrow;
    u[j+n] -= (unsigned)borrow;

    DEBUG(dbgs() << "KnuthDiv: after subtraction:");
    DEBUG(for (int i = m+n; i >=0; i--) dbgs() << " " << u[i]);
    DEBUG(dbgs() << '\n');

    // D5. [Test remainder.] Set q[j] = qp. If the result of step D4 was
    // negative, go to step D6; otherwise go on to step D7.
    q[j] = (unsigned)qp;
    if (isNeg) {
      // D6. [Add back]. The probability that this step is necessary is very
      // small, on the order of only 2/b. Make sure that test data accounts for
      // this possibility. Decrease q[j] by 1
      q[j]--;
      // and add (0v[n-1]...v[1]v[0]) to (u[j+n]u[j+n-1]...u[j+1]u[j]).
      // A carry will occur to the left of u[j+n], and it should be ignored
      // since it cancels with the borrow that occurred in D4.
      bool carry = false;
      for (unsigned i = 0; i < n; i++) {
        unsigned limit = std::min(u[j+i],v[i]);
        u[j+i] += v[i] + carry;
        carry = u[j+i] < limit || (carry && u[j+i] == limit);
      }
      u[j+n] += carry;
    }
    DEBUG(dbgs() << "KnuthDiv: after correction:");
    DEBUG(for (int i = m+n; i >=0; i--) dbgs() << " " << u[i]);
    DEBUG(dbgs() << "\nKnuthDiv: digit result = " << q[j] << '\n');

  // D7. [Loop on j.]  Decrease j by one. Now if j >= 0, go back to D3.
  } while (--j >= 0);

  DEBUG(dbgs() << "KnuthDiv: quotient:");
  DEBUG(for (int i = m; i >=0; i--) dbgs() <<" " << q[i]);
  DEBUG(dbgs() << '\n');

  // D8. [Unnormalize]. Now q[...] is the desired quotient, and the desired
  // remainder may be obtained by dividing u[...] by d. If r is non-null we
  // compute the remainder (urem uses this).
  if (r) {
    // The value d is expressed by the "shift" value above since we avoided
    // multiplication by d by using a shift left. So, all we have to do is
    // shift right here. In order to mak
    if (shift) {
      unsigned carry = 0;
      DEBUG(dbgs() << "KnuthDiv: remainder:");
      for (int i = n-1; i >= 0; i--) {
        r[i] = (u[i] >> shift) | carry;
        carry = u[i] << (32 - shift);
        DEBUG(dbgs() << " " << r[i]);
      }
    } else {
      for (int i = n-1; i >= 0; i--) {
        r[i] = u[i];
        DEBUG(dbgs() << " " << r[i]);
      }
    }
    DEBUG(dbgs() << '\n');
  }
  DEBUG(dbgs() << '\n');
}

void APIntImpl::divide(APIntRef LHS, unsigned lhsWords,
                       APIntRef RHS, unsigned rhsWords,
                       APIntImpl *Quotient, APIntImpl *Remainder) {
  assert(lhsWords >= rhsWords && "Fractional result");
  unsigned LHSBitWidth = LHS.BitWidth;
  unsigned RHSBitWidth = RHS.BitWidth;

  // First, compose the values into an array of 32-bit words instead of
  // 64-bit words. This is a necessity of both the "short division" algorithm
  // and the Knuth "classical algorithm" which requires there to be native
  // operations for +, -, and * on an m bit value with an m*2 bit result. We
  // can't use 64-bit operands here because we don't have native results of
  // 128-bits. Furthermore, casting the 64-bit values to 32-bit values won't
  // work on large-endian machines.
  uint64_t mask = ~0ull >> (sizeof(unsigned)*CHAR_BIT);
  unsigned n = rhsWords * 2;
  unsigned m = (lhsWords * 2) - n;

  // Allocate space for the temporary values we need either on the stack, if
  // it will fit, or on the heap if it won't.
  unsigned SPACE[128];
  unsigned *U = nullptr;
  unsigned *V = nullptr;
  unsigned *Q = nullptr;
  unsigned *R = nullptr;
  if ((Remainder?4:3)*n+2*m+1 <= 128) {
    U = &SPACE[0];
    V = &SPACE[m+n+1];
    Q = &SPACE[(m+n+1) + n];
    if (Remainder)
      R = &SPACE[(m+n+1) + n + (m+n)];
  } else {
    U = new unsigned[m + n + 1];
    V = new unsigned[n];
    Q = new unsigned[m+n];
    if (Remainder)
      R = new unsigned[n];
  }

  // Initialize the dividend
  memset(U, 0, (m+n+1)*sizeof(unsigned));
  for (unsigned i = 0; i < lhsWords; ++i) {
    uint64_t tmp = LHS.words()[i];
    U[i * 2] = (unsigned)(tmp & mask);
    U[i * 2 + 1] = (unsigned)(tmp >> (sizeof(unsigned)*CHAR_BIT));
  }
  U[m+n] = 0; // this extra word is for "spill" in the Knuth algorithm.

  // Initialize the divisor
  memset(V, 0, (n)*sizeof(unsigned));
  for (unsigned i = 0; i < rhsWords; ++i) {
    uint64_t tmp = RHS.words()[i];
    V[i * 2] = (unsigned)(tmp & mask);
    V[i * 2 + 1] = (unsigned)(tmp >> (sizeof(unsigned)*CHAR_BIT));
  }

  // initialize the quotient and remainder
  memset(Q, 0, (m+n) * sizeof(unsigned));
  if (Remainder)
    memset(R, 0, n * sizeof(unsigned));

  // Now, adjust m and n for the Knuth division. n is the number of words in
  // the divisor. m is the number of words by which the dividend exceeds the
  // divisor (i.e. m+n is the length of the dividend). These sizes must not
  // contain any zero words or the Knuth algorithm fails.
  for (unsigned i = n; i > 0 && V[i-1] == 0; i--) {
    n--;
    m++;
  }
  for (unsigned i = m+n; i > 0 && U[i-1] == 0; i--)
    m--;

  // If we're left with only a single word for the divisor, Knuth doesn't work
  // so we implement the short division algorithm here. This is much simpler
  // and faster because we are certain that we can divide a 64-bit quantity
  // by a 32-bit quantity at hardware speed and short division is simply a
  // series of such operations. This is just like doing short division but we
  // are using base 2^32 instead of base 10.
  assert(n != 0 && "Divide by zero?");
  if (n == 1) {
    unsigned divisor = V[0];
    unsigned remainder = 0;
    for (int i = m+n-1; i >= 0; i--) {
      uint64_t partial_dividend = uint64_t(remainder) << 32 | U[i];
      if (partial_dividend == 0) {
        Q[i] = 0;
        remainder = 0;
      } else if (partial_dividend < divisor) {
        Q[i] = 0;
        remainder = (unsigned)partial_dividend;
      } else if (partial_dividend == divisor) {
        Q[i] = 1;
        remainder = 0;
      } else {
        Q[i] = (unsigned)(partial_dividend / divisor);
        remainder = (unsigned)(partial_dividend - (Q[i] * divisor));
      }
    }
    if (R)
      R[0] = remainder;
  } else {
    // Now we're ready to invoke the Knuth classical divide algorithm. In this
    // case n > 1.
    KnuthDiv(U, V, Q, R, m, n);
  }

  // If the caller wants the quotient
  if (Quotient) {
    // Set up the Quotient value's memory.
    if (Quotient->BitWidth != LHSBitWidth) {
      if (Quotient->needsCleanup())
        delete [] Quotient->OutOfLineStorage;
      else
        Quotient->clearAllBits();
      Quotient->BitWidth = LHSBitWidth;
      if (Quotient->needsCleanup())
        Quotient->OutOfLineStorage = getClearedMemory(Quotient->getNumWords());
    } else {
      Quotient->clearAllBits();
    }

    // The quotient is in Q. Reconstitute the quotient into Quotient's low
    // order words.
    // This case is currently dead as all users of divide() handle trivial cases
    // earlier.
    if (lhsWords == 1) {
      uint64_t tmp =
        uint64_t(Q[0]) | (uint64_t(Q[1]) << (APINT_BITS_PER_WORD / 2));
      Quotient->words()[0] = tmp;
    } else {
      assert(!Quotient->isSingleWord() && "Quotient APInt not large enough");
      for (unsigned i = 0; i < lhsWords; ++i)
        Quotient->words()[i] =
          uint64_t(Q[i*2]) | (uint64_t(Q[i*2+1]) << (APINT_BITS_PER_WORD / 2));
    }
  }

  // If the caller wants the remainder
  if (Remainder) {
    // Set up the Remainder value's memory.
    if (Remainder->BitWidth != RHSBitWidth) {
      if (Remainder->needsCleanup())
        delete [] Remainder->OutOfLineStorage;
      else
        Remainder->clearAllBits();
      Remainder->BitWidth = RHSBitWidth;
      if (Remainder->needsCleanup())
        Remainder->OutOfLineStorage =
            getClearedMemory(Remainder->getNumWords());
    } else {
      Remainder->clearAllBits();
    }

    // The remainder is in R. Reconstitute the remainder into Remainder's low
    // order words.
    if (rhsWords == 1) {
      uint64_t tmp =
        uint64_t(R[0]) | (uint64_t(R[1]) << (APINT_BITS_PER_WORD / 2));
      Remainder->words()[0] = tmp;
    } else {
      assert(!Remainder->isSingleWord() && "Remainder APInt not large enough");
      for (unsigned i = 0; i < rhsWords; ++i)
        Remainder->words()[i] =
          uint64_t(R[i*2]) | (uint64_t(R[i*2+1]) << (APINT_BITS_PER_WORD / 2));
    }
  }

  // Clean up the memory we allocated.
  if (U != &SPACE[0]) {
    delete [] U;
    delete [] V;
    delete [] Q;
    delete [] R;
  }
}

void APIntImpl::udiv(APIntRef RHS) {
  assert(BitWidth == RHS.BitWidth && "Bit widths must be the same");

  // First, deal with the easy case
  if (isSingleWord()) {
    assert(RHS.words()[0] != 0 && "Divide by zero?");
    InlineStorage[0] /= RHS.words()[0];
    return;
  }

  // Get some facts about the LHS and RHS number of bits and words
  unsigned rhsBits = RHS.getActiveBits();
  unsigned rhsWords = !rhsBits ? 0 : (APIntRef::whichWord(rhsBits - 1) + 1);
  assert(rhsWords && "Divided by zero???");
  unsigned lhsBits = this->getActiveBits();
  unsigned lhsWords = !lhsBits ? 0 : (APIntRef::whichWord(lhsBits - 1) + 1);

  // Deal with some degenerate cases
  if (!lhsWords) {
    // 0 / X ===> 0
    clearAllBits();
    return;
  } else if (lhsWords < rhsWords || APIntRef(*this).ult(RHS)) {
    // X / Y ===> 0, iff X < Y
    clearAllBits();
    return;
  } else if (APIntRef(*this) == RHS) {
    // X / X ===> 1
    clearAllBits();
    words()[0] = 1;
    return;
  } else if (lhsWords == 1 && rhsWords == 1) {
    // All high words are zero, just use native divide
    clearAllBits();
    words()[0] /= RHS.words()[0];
    return;
  }

  // We have to compute it the hard way. Invoke the Knuth divide algorithm.
  APIntImpl LHSImpl(*this);
  divide(*this, lhsWords, RHS, rhsWords, &LHSImpl, nullptr);
}

void APIntImpl::urem(APIntRef RHS) {
  assert(BitWidth == RHS.BitWidth && "Bit widths must be the same");
  if (isSingleWord()) {
    assert(RHS.words()[0] != 0 && "Divide by zero?");
    InlineStorage[0] %= RHS.words()[0];
    return;
  }

  // Get some facts about the LHS
  unsigned lhsBits = getActiveBits();
  unsigned lhsWords = !lhsBits ? 0 : (APIntRef::whichWord(lhsBits - 1) + 1);

  // Get some facts about the RHS
  unsigned rhsBits = RHS.getActiveBits();
  unsigned rhsWords = !rhsBits ? 0 : (APIntRef::whichWord(rhsBits - 1) + 1);
  assert(rhsWords && "Performing remainder operation by zero ???");

  // Check the degenerate cases
  if (lhsWords == 0) {
    // 0 % Y ===> 0
    clearAllBits();
    return;
  } else if (lhsWords < rhsWords || APIntRef(*this).ult(RHS)) {
    // X % Y ===> X, iff X < Y
    return;
  } else if (APIntRef(*this) == RHS) {
    // X % X == 0;
    clearAllBits();
    return;
  } else if (lhsWords == 1) {
    // All high words are zero, just use native remainder
    clearAllBits();
    words()[0] %= RHS.words()[0];
    return;
  }

  // We have to compute it the hard way. Invoke the Knuth divide algorithm.
  APIntImpl LHSImpl(*this);
  divide(*this, lhsWords, RHS, rhsWords, nullptr, &LHSImpl);
}

void APIntImpl::udivrem(APIntRef LHS, APIntRef RHS,
                        APIntImpl Quotient, APIntImpl Remainder) {
  assert(LHS.BitWidth == RHS.BitWidth && "Bit widths must be the same");

  // First, deal with the easy case
  if (LHS.isSingleWord()) {
    assert(RHS.words()[0] != 0 && "Divide by zero?");
    uint64_t QuotVal = LHS.words()[0] / RHS.words()[0];
    uint64_t RemVal = LHS.words()[0] % RHS.words()[0];
    Quotient.InlineStorage[0] = QuotVal;
    Remainder.InlineStorage[0] = RemVal;
    return;
  }

  // Get some size facts about the dividend and divisor
  unsigned lhsBits  = LHS.getActiveBits();
  unsigned lhsWords = !lhsBits ? 0 : (APIntRef::whichWord(lhsBits - 1) + 1);
  unsigned rhsBits  = RHS.getActiveBits();
  unsigned rhsWords = !rhsBits ? 0 : (APIntRef::whichWord(rhsBits - 1) + 1);

  // Check the degenerate cases
  if (lhsWords == 0) {
    Quotient.clearAllBits();  // 0 / Y ===> 0
    Remainder.clearAllBits(); // 0 % Y ===> 0
    return;
  }

  if (lhsWords < rhsWords || LHS.ult(RHS)) {
    // X % Y ===> X, iff X < Y
    std::copy(LHS.words().begin(), LHS.words().end(),
              Remainder.words().begin());
    // X / Y ===> 0, iff X < Y
    Quotient.clearAllBits();
    return;
  }

  if (LHS == RHS) {
    // X / X ===> 1
    Quotient.clearAllBits();
    Quotient.words()[0] = 1;
    // X % X ===> 0;
    Remainder.clearAllBits();
    return;
  }

  if (lhsWords == 1 && rhsWords == 1) {
    // There is only one word to consider so use the native versions.
    uint64_t lhsValue = LHS.words()[0];
    uint64_t rhsValue = RHS.words()[0];
    Quotient.clearAllBits();
    Quotient.words()[0] = lhsValue / rhsValue;
    Remainder.clearAllBits();
    Remainder.words()[0] = lhsValue % rhsValue;
    return;
  }

  // Okay, lets do it the long way
  divide(LHS, lhsWords, RHS, rhsWords, &Quotient, &Remainder);
}

void APIntImpl::fromString(unsigned numbits, StringRef str, uint8_t radix) {
  // Check our assumptions here
  assert(!str.empty() && "Invalid string length");
  assert((radix == 10 || radix == 8 || radix == 16 || radix == 2 || 
          radix == 36) &&
         "Radix should be 2, 8, 10, 16, or 36!");

  StringRef::iterator p = str.begin();
  size_t slen = str.size();
  bool isNeg = *p == '-';
  if (*p == '-' || *p == '+') {
    p++;
    slen--;
    assert(slen && "String is only a sign, needs a value.");
  }
  assert((slen <= numbits || radix != 2) && "Insufficient bit width");
  assert(((slen-1)*3 <= numbits || radix != 8) && "Insufficient bit width");
  assert(((slen-1)*4 <= numbits || radix != 16) && "Insufficient bit width");
  assert((((slen-1)*64)/22 <= numbits || radix != 10) &&
         "Insufficient bit width");

  // Allocate memory
  if (needsCleanup())
    OutOfLineStorage = getMemory(getNumWords());
  clearAllBits();

  // Figure out if we can shift instead of multiply
  unsigned shift = (radix == 16 ? 4 : radix == 8 ? 3 : radix == 2 ? 1 : 0);

  // Set up an APInt for the digit to add outside the loop so we don't
  // constantly construct/destruct it.
  APInt apdigit(getBitWidth(), 0);
  APInt apradix(getBitWidth(), radix);

  // Enter digit traversal loop
  for (StringRef::iterator e = str.end(); p != e; ++p) {
    unsigned digit = getDigit(*p, radix);
    assert(digit < radix && "Invalid character in digit string");

    // Shift or multiply the value by the radix
    if (slen > 1) {
      if (shift)
        this->shl(shift);
      else
        this->multiply(apradix);
    }

    // Add in the digit we just interpreted
    apdigit.words()[0] = digit;
    this->add(apdigit);
  }
  // If its negative, put it in two's complement form
  if (isNeg) {
    this->decrement();
    this->flipAllBits();
  }
}

void APIntRef::toString(SmallVectorImpl<char> &Str, unsigned Radix, bool Signed,
                        bool formatAsCLiteral) const {
  assert((Radix == 10 || Radix == 8 || Radix == 16 || Radix == 2 || 
          Radix == 36) &&
         "Radix should be 2, 8, 10, 16, or 36!");

  const char *Prefix = "";
  if (formatAsCLiteral) {
    switch (Radix) {
      case 2:
        // Binary literals are a non-standard extension added in gcc 4.3:
        // http://gcc.gnu.org/onlinedocs/gcc-4.3.0/gcc/Binary-constants.html
        Prefix = "0b";
        break;
      case 8:
        Prefix = "0";
        break;
      case 10:
        break; // No prefix
      case 16:
        Prefix = "0x";
        break;
      default:
        llvm_unreachable("Invalid radix!");
    }
  }

  // First, check for a zero value and just short circuit the logic below.
  if (*this == 0) {
    while (*Prefix) {
      Str.push_back(*Prefix);
      ++Prefix;
    };
    Str.push_back('0');
    return;
  }

  static const char Digits[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

  if (isSingleWord()) {
    char Buffer[65];
    char *BufPtr = Buffer+65;

    uint64_t N;
    if (!Signed) {
      N = getZExtValue();
    } else {
      int64_t I = getSExtValue();
      if (I >= 0) {
        N = I;
      } else {
        Str.push_back('-');
        N = -(uint64_t)I;
      }
    }

    while (*Prefix) {
      Str.push_back(*Prefix);
      ++Prefix;
    };

    while (N) {
      *--BufPtr = Digits[N % Radix];
      N /= Radix;
    }
    Str.append(BufPtr, Buffer+65);
    return;
  }

  APInt Tmp(*this);

  if (Signed && isNegative()) {
    // They want to print the signed version and it is a negative value
    // Flip the bits and add one to turn it into the equivalent positive
    // value and put a '-' in the result.
    Tmp.flipAllBits();
    ++Tmp;
    Str.push_back('-');
  }

  while (*Prefix) {
    Str.push_back(*Prefix);
    ++Prefix;
  };

  // We insert the digits backward, then reverse them to get the right order.
  unsigned StartDig = Str.size();

  // For the 2, 8 and 16 bit cases, we can just shift instead of divide
  // because the number of bits per digit (1, 3 and 4 respectively) divides
  // equaly.  We just shift until the value is zero.
  if (Radix == 2 || Radix == 8 || Radix == 16) {
    // Just shift tmp right for each digit width until it becomes zero
    unsigned ShiftAmt = (Radix == 16 ? 4 : (Radix == 8 ? 3 : 1));
    unsigned MaskAmt = Radix - 1;

    while (Tmp != 0) {
      unsigned Digit = unsigned(Tmp.getRawData()[0]) & MaskAmt;
      Str.push_back(Digits[Digit]);
      Tmp = Tmp.lshr(ShiftAmt);
    }
  } else {
    APInt divisor(Radix == 10? 4 : 8, Radix);
    while (Tmp != 0) {
      APInt APdigit(1, 0);
      APInt tmp2(Tmp.getBitWidth(), 0);
      APIntImpl tmp2ref(tmp2);
      APIntImpl APdigitref(APdigit);
      APIntImpl::divide(Tmp, Tmp.getNumWords(), divisor, divisor.getNumWords(),
                        &tmp2ref, &APdigitref);
      unsigned Digit = (unsigned)APdigit.getZExtValue();
      assert(Digit < Radix && "divide failed");
      Str.push_back(Digits[Digit]);
      Tmp = tmp2;
    }
  }

  // Reverse the digits before returning.
  std::reverse(Str.begin()+StartDig, Str.end());
}

/// Returns the APInt as a std::string. Note that this is an inefficient method.
/// It is better to pass in a SmallVector/SmallString to the methods above.
std::string APIntRef::toString(unsigned Radix = 10, bool Signed = true) const {
  SmallString<40> S;
  toString(S, Radix, Signed, /* formatAsCLiteral = */false);
  return S.str();
}


LLVM_DUMP_METHOD void APIntRef::dump() const {
  SmallString<40> S, U;
  this->toString(U, /*Radix=*/10, /*Signed=*/false, /*formatAsCLiteral=*/false);
  this->toString(S, /*Radix=*/10, /*Signed=*/true, /*formatAsCLiteral=*/false);
  dbgs() << "APInt(" << BitWidth << "b, "
         << U << "u " << S << "s)";
}

void APIntRef::print(raw_ostream &OS, bool isSigned) const {
  SmallString<40> S;
  this->toString(S, 10, isSigned, /* formatAsCLiteral = */false);
  OS << S;
}

// This implements a variety of operations on a representation of
// arbitrary precision, two's-complement, bignum integer values.

// Assumed by lowHalf, highHalf, partMSB and partLSB.  A fairly safe
// and unrestricting assumption.
static_assert(integerPartWidth % 2 == 0, "Part width must be divisible by 2!");

/* Some handy functions local to this file.  */
namespace {

  /* Returns the integer part with the least significant BITS set.
     BITS cannot be zero.  */
  static inline integerPart
  lowBitMask(unsigned int bits)
  {
    assert(bits != 0 && bits <= integerPartWidth);

    return ~(integerPart) 0 >> (integerPartWidth - bits);
  }

  /* Returns the value of the lower half of PART.  */
  static inline integerPart
  lowHalf(integerPart part)
  {
    return part & lowBitMask(integerPartWidth / 2);
  }

  /* Returns the value of the upper half of PART.  */
  static inline integerPart
  highHalf(integerPart part)
  {
    return part >> (integerPartWidth / 2);
  }

  /* Returns the bit number of the most significant set bit of a part.
     If the input number has no bits set -1U is returned.  */
  static unsigned int
  partMSB(integerPart value)
  {
    return findLastSet(value, ZB_Max);
  }

  /* Returns the bit number of the least significant set bit of a
     part.  If the input number has no bits set -1U is returned.  */
  static unsigned int
  partLSB(integerPart value)
  {
    return findFirstSet(value, ZB_Max);
  }
}

/* Sets the least significant part of a bignum to the input value, and
   zeroes out higher parts.  */
void
tc::Set(integerPart *dst, integerPart part, unsigned int parts)
{
  unsigned int i;

  assert(parts > 0);

  dst[0] = part;
  for (i = 1; i < parts; i++)
    dst[i] = 0;
}

/* Assign one bignum to another.  */
void
tc::Assign(integerPart *dst, const integerPart *src, unsigned int parts)
{
  unsigned int i;

  for (i = 0; i < parts; i++)
    dst[i] = src[i];
}

/* Returns true if a bignum is zero, false otherwise.  */
bool
tc::IsZero(const integerPart *src, unsigned int parts)
{
  unsigned int i;

  for (i = 0; i < parts; i++)
    if (src[i])
      return false;

  return true;
}

/* Extract the given bit of a bignum; returns 0 or 1.  */
int
tc::ExtractBit(const integerPart *parts, unsigned int bit)
{
  return (parts[bit / integerPartWidth] &
          ((integerPart) 1 << bit % integerPartWidth)) != 0;
}

/* Set the given bit of a bignum. */
void
tc::SetBit(integerPart *parts, unsigned int bit)
{
  parts[bit / integerPartWidth] |= (integerPart) 1 << (bit % integerPartWidth);
}

/* Clears the given bit of a bignum. */
void
tc::ClearBit(integerPart *parts, unsigned int bit)
{
  parts[bit / integerPartWidth] &=
    ~((integerPart) 1 << (bit % integerPartWidth));
}

/* Returns the bit number of the least significant set bit of a
   number.  If the input number has no bits set -1U is returned.  */
unsigned int
tc::LSB(const integerPart *parts, unsigned int n)
{
  unsigned int i, lsb;

  for (i = 0; i < n; i++) {
      if (parts[i] != 0) {
          lsb = partLSB(parts[i]);

          return lsb + i * integerPartWidth;
      }
  }

  return -1U;
}

/* Returns the bit number of the most significant set bit of a number.
   If the input number has no bits set -1U is returned.  */
unsigned int
tc::MSB(const integerPart *parts, unsigned int n)
{
  unsigned int msb;

  do {
    --n;

    if (parts[n] != 0) {
      msb = partMSB(parts[n]);

      return msb + n * integerPartWidth;
    }
  } while (n);

  return -1U;
}

/* Copy the bit vector of width srcBITS from SRC, starting at bit
   srcLSB, to DST, of dstCOUNT parts, such that the bit srcLSB becomes
   the least significant bit of DST.  All high bits above srcBITS in
   DST are zero-filled.  */
void tc::Extract(integerPart *dst, unsigned int dstCount,
                 const integerPart *src, unsigned int srcBits,
                 unsigned int srcLSB) {
  unsigned int firstSrcPart, dstParts, shift, n;

  dstParts = (srcBits + integerPartWidth - 1) / integerPartWidth;
  assert(dstParts <= dstCount);

  firstSrcPart = srcLSB / integerPartWidth;
  Assign (dst, src + firstSrcPart, dstParts);

  shift = srcLSB % integerPartWidth;
  ShiftRight (dst, dstParts, shift);

  /* We now have (dstParts * integerPartWidth - shift) bits from SRC
     in DST.  If this is less that srcBits, append the rest, else
     clear the high bits.  */
  n = dstParts * integerPartWidth - shift;
  if (n < srcBits) {
    integerPart mask = lowBitMask (srcBits - n);
    dst[dstParts - 1] |= ((src[firstSrcPart + dstParts] & mask)
                          << n % integerPartWidth);
  } else if (n > srcBits) {
    if (srcBits % integerPartWidth)
      dst[dstParts - 1] &= lowBitMask (srcBits % integerPartWidth);
  }

  /* Clear high parts.  */
  while (dstParts < dstCount)
    dst[dstParts++] = 0;
}

/* DST += RHS + C where C is zero or one.  Returns the carry flag.  */
integerPart tc::Add(integerPart *dst, const integerPart *rhs, integerPart c,
                    unsigned int parts) {
  unsigned int i;

  assert(c <= 1);

  for (i = 0; i < parts; i++) {
    integerPart l;

    l = dst[i];
    if (c) {
      dst[i] += rhs[i] + 1;
      c = (dst[i] <= l);
    } else {
      dst[i] += rhs[i];
      c = (dst[i] < l);
    }
  }

  return c;
}

/* DST -= RHS + C where C is zero or one.  Returns the carry flag.  */
integerPart tc::Subtract(integerPart *dst, const integerPart *rhs,
                         integerPart c, unsigned int parts) {
  unsigned int i;

  assert(c <= 1);

  for (i = 0; i < parts; i++) {
    integerPart l;

    l = dst[i];
    if (c) {
      dst[i] -= rhs[i] + 1;
      c = (dst[i] >= l);
    } else {
      dst[i] -= rhs[i];
      c = (dst[i] > l);
    }
  }

  return c;
}

/* Negate a bignum in-place.  */
void
tc::Negate(integerPart *dst, unsigned int parts)
{
  Complement(dst, parts);
  Increment(dst, parts);
}

/*  DST += SRC * MULTIPLIER + CARRY   if add is true
    DST  = SRC * MULTIPLIER + CARRY   if add is false

    Requires 0 <= DSTPARTS <= SRCPARTS + 1.  If DST overlaps SRC
    they must start at the same point, i.e. DST == SRC.

    If DSTPARTS == SRCPARTS + 1 no overflow occurs and zero is
    returned.  Otherwise DST is filled with the least significant
    DSTPARTS parts of the result, and if all of the omitted higher
    parts were zero return zero, otherwise overflow occurred and
    return one.  */
int tc::MultiplyPart(integerPart *dst, const integerPart *src,
                     integerPart multiplier, integerPart carry,
                     unsigned int srcParts, unsigned int dstParts, bool add) {
  unsigned int i, n;

  /* Otherwise our writes of DST kill our later reads of SRC.  */
  assert(dst <= src || dst >= src + srcParts);
  assert(dstParts <= srcParts + 1);

  /* N loops; minimum of dstParts and srcParts.  */
  n = dstParts < srcParts ? dstParts: srcParts;

  for (i = 0; i < n; i++) {
    integerPart low, mid, high, srcPart;

      /* [ LOW, HIGH ] = MULTIPLIER * SRC[i] + DST[i] + CARRY.

         This cannot overflow, because

         (n - 1) * (n - 1) + 2 (n - 1) = (n - 1) * (n + 1)

         which is less than n^2.  */

    srcPart = src[i];

    if (multiplier == 0 || srcPart == 0)        {
      low = carry;
      high = 0;
    } else {
      low = lowHalf(srcPart) * lowHalf(multiplier);
      high = highHalf(srcPart) * highHalf(multiplier);

      mid = lowHalf(srcPart) * highHalf(multiplier);
      high += highHalf(mid);
      mid <<= integerPartWidth / 2;
      if (low + mid < low)
        high++;
      low += mid;

      mid = highHalf(srcPart) * lowHalf(multiplier);
      high += highHalf(mid);
      mid <<= integerPartWidth / 2;
      if (low + mid < low)
        high++;
      low += mid;

      /* Now add carry.  */
      if (low + carry < low)
        high++;
      low += carry;
    }

    if (add) {
      /* And now DST[i], and store the new low part there.  */
      if (low + dst[i] < low)
        high++;
      dst[i] += low;
    } else
      dst[i] = low;

    carry = high;
  }

  if (i < dstParts) {
    /* Full multiplication, there is no overflow.  */
    assert(i + 1 == dstParts);
    dst[i] = carry;
    return 0;
  } else {
    /* We overflowed if there is carry.  */
    if (carry)
      return 1;

    /* We would overflow if any significant unwritten parts would be
       non-zero.  This is true if any remaining src parts are non-zero
       and the multiplier is non-zero.  */
    if (multiplier)
      for (; i < srcParts; i++)
        if (src[i])
          return 1;

    /* We fitted in the narrow destination.  */
    return 0;
  }
}

/* DST = LHS * RHS, where DST has the same width as the operands and
   is filled with the least significant parts of the result.  Returns
   one if overflow occurred, otherwise zero.  DST must be disjoint
   from both operands.  */
int tc::Multiply(integerPart *dst, const integerPart *lhs,
                 const integerPart *rhs, unsigned int parts) {
  unsigned int i;
  int overflow;

  assert(dst != lhs && dst != rhs);

  overflow = 0;
  Set(dst, 0, parts);

  for (i = 0; i < parts; i++)
    overflow |= MultiplyPart(&dst[i], lhs, rhs[i], 0, parts, parts - i, true);

  return overflow;
}

/* DST = LHS * RHS, where DST has width the sum of the widths of the
   operands.  No overflow occurs.  DST must be disjoint from both
   operands.  Returns the number of parts required to hold the
   result.  */
unsigned int tc::FullMultiply(integerPart *dst, const integerPart *lhs,
                              const integerPart *rhs, unsigned int lhsParts,
                              unsigned int rhsParts) {
  /* Put the narrower number on the LHS for less loops below.  */
  if (lhsParts > rhsParts) {
    return FullMultiply(dst, rhs, lhs, rhsParts, lhsParts);
  } else {
    unsigned int n;

    assert(dst != lhs && dst != rhs);

    Set(dst, 0, rhsParts);

    for (n = 0; n < lhsParts; n++)
      MultiplyPart(&dst[n], rhs, lhs[n], 0, rhsParts, rhsParts + 1, true);

    n = lhsParts + rhsParts;

    return n - (dst[n - 1] == 0);
  }
}

/* If RHS is zero LHS and REMAINDER are left unchanged, return one.
   Otherwise set LHS to LHS / RHS with the fractional part discarded,
   set REMAINDER to the remainder, return zero.  i.e.

   OLD_LHS = RHS * LHS + REMAINDER

   SCRATCH is a bignum of the same size as the operands and result for
   use by the routine; its contents need not be initialized and are
   destroyed.  LHS, REMAINDER and SCRATCH must be distinct.
*/
int tc::Divide(integerPart *lhs, const integerPart *rhs, integerPart *remainder,
               integerPart *srhs, unsigned int parts) {
  unsigned int n, shiftCount;
  integerPart mask;

  assert(lhs != remainder && lhs != srhs && remainder != srhs);

  shiftCount = MSB(rhs, parts) + 1;
  if (shiftCount == 0)
    return true;

  shiftCount = parts * integerPartWidth - shiftCount;
  n = shiftCount / integerPartWidth;
  mask = (integerPart) 1 << (shiftCount % integerPartWidth);

  Assign(srhs, rhs, parts);
  ShiftLeft(srhs, parts, shiftCount);
  Assign(remainder, lhs, parts);
  Set(lhs, 0, parts);

  /* Loop, subtracting SRHS if REMAINDER is greater and adding that to
     the total.  */
  for (;;) {
      int compare;

      compare = Compare(remainder, srhs, parts);
      if (compare >= 0) {
        Subtract(remainder, srhs, 0, parts);
        lhs[n] |= mask;
      }

      if (shiftCount == 0)
        break;
      shiftCount--;
      ShiftRight(srhs, parts, 1);
      if ((mask >>= 1) == 0) {
        mask = (integerPart) 1 << (integerPartWidth - 1);
        n--;
      }
  }

  return false;
}

/* Shift a bignum left COUNT bits in-place.  Shifted in bits are zero.
   There are no restrictions on COUNT.  */
void
tc::ShiftLeft(integerPart *dst, unsigned int parts, unsigned int count)
{
  if (count) {
    unsigned int jump, shift;

    /* Jump is the inter-part jump; shift is is intra-part shift.  */
    jump = count / integerPartWidth;
    shift = count % integerPartWidth;

    while (parts > jump) {
      integerPart part;

      parts--;

      /* dst[i] comes from the two parts src[i - jump] and, if we have
         an intra-part shift, src[i - jump - 1].  */
      part = dst[parts - jump];
      if (shift) {
        part <<= shift;
        if (parts >= jump + 1)
          part |= dst[parts - jump - 1] >> (integerPartWidth - shift);
      }

      dst[parts] = part;
    }

    while (parts > 0)
      dst[--parts] = 0;
  }
}

/* Shift a bignum right COUNT bits in-place.  Shifted in bits are
   zero.  There are no restrictions on COUNT.  */
void
tc::ShiftRight(integerPart *dst, unsigned int parts, unsigned int count)
{
  if (count) {
    unsigned int i, jump, shift;

    /* Jump is the inter-part jump; shift is is intra-part shift.  */
    jump = count / integerPartWidth;
    shift = count % integerPartWidth;

    /* Perform the shift.  This leaves the most significant COUNT bits
       of the result at zero.  */
    for (i = 0; i < parts; i++) {
      integerPart part;

      if (i + jump >= parts) {
        part = 0;
      } else {
        part = dst[i + jump];
        if (shift) {
          part >>= shift;
          if (i + jump + 1 < parts)
            part |= dst[i + jump + 1] << (integerPartWidth - shift);
        }
      }

      dst[i] = part;
    }
  }
}

/* Bitwise and of two bignums.  */
void
tc::And(integerPart *dst, const integerPart *rhs, unsigned int parts)
{
  unsigned int i;

  for (i = 0; i < parts; i++)
    dst[i] &= rhs[i];
}

/* Bitwise inclusive or of two bignums.  */
void
tc::Or(integerPart *dst, const integerPart *rhs, unsigned int parts)
{
  unsigned int i;

  for (i = 0; i < parts; i++)
    dst[i] |= rhs[i];
}

/* Bitwise exclusive or of two bignums.  */
void
tc::Xor(integerPart *dst, const integerPart *rhs, unsigned int parts)
{
  unsigned int i;

  for (i = 0; i < parts; i++)
    dst[i] ^= rhs[i];
}

/* Complement a bignum in-place.  */
void
tc::Complement(integerPart *dst, unsigned int parts)
{
  unsigned int i;

  for (i = 0; i < parts; i++)
    dst[i] = ~dst[i];
}

/* Comparison (unsigned) of two bignums.  */
int tc::Compare(const integerPart *lhs, const integerPart *rhs,
                unsigned int parts) {
  while (parts) {
      parts--;
      if (lhs[parts] == rhs[parts])
        continue;

      if (lhs[parts] > rhs[parts])
        return 1;
      else
        return -1;
    }

  return 0;
}

/* Increment a bignum in-place, return the carry flag.  */
integerPart
tc::Increment(integerPart *dst, unsigned int parts)
{
  unsigned int i;

  for (i = 0; i < parts; i++)
    if (++dst[i] != 0)
      break;

  return i == parts;
}

/* Decrement a bignum in-place, return the borrow flag.  */
integerPart
tc::Decrement(integerPart *dst, unsigned int parts) {
  for (unsigned int i = 0; i < parts; i++) {
    // If the current word is non-zero, then the decrement has no effect on the
    // higher-order words of the integer and no borrow can occur. Exit early.
    if (dst[i]--)
      return 0;
  }
  // If every word was zero, then there is a borrow.
  return 1;
}


/* Set the least significant BITS bits of a bignum, clear the
   rest.  */
void tc::SetLeastSignificantBits(integerPart *dst, unsigned int parts,
                                 unsigned int bits) {
  unsigned int i;

  i = 0;
  while (bits > integerPartWidth) {
    dst[i++] = ~(integerPart) 0;
    bits -= integerPartWidth;
  }

  if (bits)
    dst[i++] = ~(integerPart) 0 >> (integerPartWidth - bits);

  while (i < parts)
    dst[i++] = 0;
}
