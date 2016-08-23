//===-- llvm/ADT/APInt.h - For Arbitrary Precision Integer -----*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file implements a class to represent arbitrary precision
/// integral constant values and operations on them.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_ADT_APINT_H
#define LLVM_ADT_APINT_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/MathExtras.h"
#include <algorithm>
#include <cassert>
#include <climits>
#include <cstring>
#include <string>

namespace llvm {
class FoldingSetNodeID;
class StringRef;
class hash_code;
class raw_ostream;

template <typename T> class SmallVectorImpl;
template <typename T> class ArrayRef;

// An unsigned host type used as a single part of a multi-part
// bignum.
typedef uint64_t integerPart;

const unsigned int host_char_bit = 8;
const unsigned int integerPartWidth =
    host_char_bit * static_cast<unsigned int>(sizeof(integerPart));

template <unsigned NumInlineWords>
class GenericAPInt;

template <unsigned NumInlineWords>
inline GenericAPInt<NumInlineWords> operator-(GenericAPInt<NumInlineWords>);

class APIntImpl;
class APIntRef;

/// This enum is used to hold the constants we needed for GenericAPInt.
enum {
  /// Bits in a word
  APINT_BITS_PER_WORD = static_cast<unsigned int>(sizeof(uint64_t)) * CHAR_BIT,
  /// Byte size of a word
  APINT_WORD_SIZE = static_cast<unsigned int>(sizeof(uint64_t)),
};

class APIntRef {
  template <unsigned NumInlineWords>
  friend class GenericAPInt;
  friend class APIntImpl;

  template <unsigned NumInlineWords>
  APIntRef(const GenericAPInt<NumInlineWords> &ConcreteAPInt)
      : Words(ConcreteAPInt.words()), BitWidth(ConcreteAPInt.getBitWidth()) {}

  inline APIntRef(const APIntImpl &Impl);

  /// \brief Get the number of words.
  ///
  /// *NOTE* Here one word's bitwidth equals to that of uint64_t.
  ///
  /// \returns the number of words to hold the integer value with a given bit
  /// width.
  unsigned getNumWords() const {
    return alignTo<APINT_BITS_PER_WORD>(BitWidth) / APINT_BITS_PER_WORD;
  }

  /// \brief Determine if this GenericAPInt just has one word to store value.
  ///
  /// \returns true if the number of bits <= 64, false otherwise.
  bool isSingleWord() const { return BitWidth <= APINT_BITS_PER_WORD; }

  ArrayRef<uint64_t> words() const { return Words; }

  /// \brief Return the number of bits in the GenericAPInt.
  unsigned getBitWidth() const { return BitWidth; }

  /// Used to insert GenericAPInt objects, or objects that contain GenericAPInt
  ///  objects, into FoldingSets.
  void Profile(FoldingSetNodeID &ID) const;

  friend hash_code hash_value(APIntRef Arg);

  /// \brief Get bits required for string value.
  ///
  /// This method determines how many bits are required to hold the APInt
  /// equivalent of the string given by \p str.
  static unsigned getBitsNeeded(StringRef str, uint8_t radix);

  /// out-of-line slow case for countLeadingZeros
  unsigned countLeadingZerosSlowCase() const;

  /// out-of-line slow case for countTrailingOnes
  unsigned countTrailingOnesSlowCase() const;

  /// out-of-line slow case for countPopulation
  unsigned countPopulationSlowCase() const;

  /// \brief The GenericAPInt version of the countLeadingZeros functions in
  ///   MathExtras.h.
  ///
  /// It counts the number of zeros from the most significant bit to the first
  /// one bit.
  ///
  /// \returns BitWidth if the value is zero, otherwise returns the number of
  ///   zeros from the most significant bit to the first one bits.
  unsigned countLeadingZeros() const {
    if (isSingleWord()) {
      unsigned unusedBits = APINT_BITS_PER_WORD - BitWidth;
      return llvm::countLeadingZeros(words().front()) - unusedBits;
    }
    return countLeadingZerosSlowCase();
  }

  /// \brief Count the number of leading one bits.
  ///
  /// This function is an GenericAPInt version of the countLeadingOnes
  /// functions in MathExtras.h. It counts the number of ones from the most
  /// significant bit to the first zero bit.
  ///
  /// \returns 0 if the high order bit is not set, otherwise returns the number
  /// of 1 bits from the most significant to the least
  unsigned countLeadingOnes() const;

  /// \brief Count the number of trailing zero bits.
  ///
  /// This function is an GenericAPInt version of the countTrailingZeros
  /// functions in MathExtras.h. It counts the number of zeros from the least
  /// significant bit to the first set bit.
  ///
  /// \returns BitWidth if the value is zero, otherwise returns the number of
  /// zeros from the least significant bit to the first one bit.
  unsigned countTrailingZeros() const;

  /// out-of-line slow case for operator==
  bool EqualSlowCase(APIntRef RHS) const;

  /// out-of-line slow case for operator==
  bool EqualSlowCase(uint64_t Val) const;

  bool operator==(APIntRef RHS) const {
    assert(BitWidth == RHS.BitWidth && "Comparison requires equal bit widths");
    if (isSingleWord())
      return words().front() == RHS.words().front();
    return EqualSlowCase(RHS);
  }

  /// \brief Equality operator.
  ///
  /// Compares this GenericAPInt with a uint64_t for the validity of the equality
  /// relationship.
  ///
  /// \returns true if *this == Val
  bool operator==(uint64_t Val) const {
    if (isSingleWord())
      return words().front() == Val;
    return APIntRef(*this).EqualSlowCase(Val);
  }

  /// \brief Unsigned less than comparison
  ///
  /// Regards both *this and RHS as unsigned quantities and compares them for
  /// the validity of the less-than relationship.
  ///
  /// \returns true if *this < RHS when both are considered unsigned.
  bool ult(APIntRef RHS) const;

  /// \brief Signed less than comparison
  ///
  /// Regards both *this and RHS as signed quantities and compares them for
  /// validity of the less-than relationship.
  ///
  /// \returns true if *this < RHS when both are considered signed.
  bool slt(APIntRef RHS) const;

  /// \brief Compute the number of active bits in the value
  ///
  /// This function returns the number of active bits which is defined as the
  /// bit width minus the number of leading zeros. This is used in several
  /// computations to see how "wide" the value is.
  unsigned getActiveBits() const { return BitWidth - countLeadingZeros(); }

  /// \brief Get the minimum bit size for this signed GenericAPInt
  ///
  /// Computes the minimum bit width for this GenericAPInt while considering it to be a
  /// signed (and probably negative) value. If the value is not negative, this
  /// function returns the same value as getActiveBits()+1. Otherwise, it
  /// returns the smallest bit width that will retain the negative value. For
  /// example, -1 can be written as 0b1 or 0xFFFFFFFFFF. 0b1 is shorter and so
  /// for -1, this function will always return 1.
  unsigned getMinSignedBits() const {
    if (isNegative())
      return BitWidth - countLeadingOnes() + 1;
    return getActiveBits() + 1;
  }

  /// \brief Get zero extended value
  ///
  /// This method attempts to return the value of this GenericAPInt as a zero extended
  /// uint64_t. The bitwidth must be <= 64 or the value must fit within a
  /// uint64_t. Otherwise an assertion will result.
  uint64_t getZExtValue() const {
    assert((isSingleWord() || getActiveBits() <= 64) &&
           "Too many bits for uint64_t");
    return words()[0];
  }

  /// \brief Get sign extended value
  ///
  /// This method attempts to return the value of this GenericAPInt as a sign extended
  /// int64_t. The bit width must be <= 64 or the value must fit within an
  /// int64_t. Otherwise an assertion will result.
  int64_t getSExtValue() const {
    if (isSingleWord())
      return int64_t(words()[0] << (APINT_BITS_PER_WORD - BitWidth)) >>
             (APINT_BITS_PER_WORD - BitWidth);
    assert(getMinSignedBits() <= 64 && "Too many bits for int64_t");
    return int64_t(words()[0]);
  }

  /// \brief Determine which word a bit is in.
  ///
  /// \returns the word position for the specified bit position.
  static unsigned whichWord(unsigned bitPosition) {
    return bitPosition / APINT_BITS_PER_WORD;
  }

  /// \brief Determine which bit in a word a bit is in.
  ///
  /// \returns the bit position in a word for the specified bit position
  /// in the GenericAPInt.
  static unsigned whichBit(unsigned bitPosition) {
    return bitPosition % APINT_BITS_PER_WORD;
  }

  /// \brief Get a single bit mask.
  ///
  /// \returns a uint64_t with only bit at "whichBit(bitPosition)" set
  /// This method generates and returns a uint64_t (word) mask for a single
  /// bit at a specific bit position. This is used to mask the bit in the
  /// corresponding word.
  static uint64_t maskBit(unsigned bitPosition) {
    return 1ULL << whichBit(bitPosition);
  }

  /// \brief Array-indexing support.
  ///
  /// \returns the bit value at bitPosition
  bool operator[](unsigned bitPosition) const {
    assert(bitPosition < getBitWidth() && "Bit position out of bounds!");
    return (maskBit(bitPosition) &
            (isSingleWord() ? words().front()
                            : words()[whichWord(bitPosition)])) != 0;
  }

  /// \brief Determine sign of this GenericAPInt.
  ///
  /// This tests the high bit of this GenericAPInt to determine if it is set.
  ///
  /// \returns true if this GenericAPInt is negative, false otherwise
  bool isNegative() const { return (*this)[BitWidth - 1]; }

  /// \brief Determine if this GenericAPInt Value is non-negative (>= 0)
  ///
  /// This tests the high bit of the GenericAPInt to determine if it is unset.
  bool isNonNegative() const { return !isNegative(); }

  /// \brief Converts this GenericAPInt to a double value.
  double roundToDouble(bool isSigned) const;

  /// Converts an APIntRef to a string and append it to Str.  Str is commonly a
  /// SmallString.
  void toString(SmallVectorImpl<char> &Str, unsigned Radix, bool Signed,
                bool formatAsCLiteral) const;

  /// \brief Return the GenericAPInt as a std::string.
  ///
  /// Note that this is an inefficient method.  It is better to pass in a
  /// SmallVector/SmallString to the methods above to avoid thrashing the heap
  /// for the string.
  std::string toString(unsigned Radix, bool Signed) const;

  void print(raw_ostream &OS, bool isSigned) const;

  /// \brief debug method
  void dump() const;

  ArrayRef<uint64_t> Words;
  unsigned BitWidth;
};

class APIntImpl {
  template <unsigned NumInlineWords>
  friend class GenericAPInt;
  friend class APIntRef;

  template <unsigned NumInlineWords>
  APIntImpl(GenericAPInt<NumInlineWords> &ConcreteAPInt)
      : InlineStorage(ConcreteAPInt.VAL), OutOfLineStorage(ConcreteAPInt.pVal),
        BitWidth(ConcreteAPInt.BitWidth), NumInlineWords(NumInlineWords) {}

  /// out-of-line slow case for inline constructor
  void initSlowCase(uint64_t Val, bool IsSigned);

  /// out-of-line slow case for inline copy constructor
  void initSlowCase(APIntRef that);

  /// shared code between two array constructors
  void initFromArray(ArrayRef<uint64_t> Array);

  unsigned getNumInlineBits() const {
    return NumInlineWords * APINT_BITS_PER_WORD;
  }

  unsigned getBitWidth() const { return BitWidth; }

  /// \brief Returns whether this instance allocated memory.
  bool needsCleanup() const { return BitWidth > getNumInlineBits(); }

  bool wouldNeedCleanup(unsigned BitWidth) {
    return BitWidth > getNumInlineBits();
  }

  /// \brief Get the number of words.
  ///
  /// *NOTE* Here one word's bitwidth equals to that of uint64_t.
  ///
  /// \returns the number of words to hold the integer value with a given bit
  /// width.
  unsigned getNumWords() const {
    return alignTo<APINT_BITS_PER_WORD>(BitWidth) / APINT_BITS_PER_WORD;
  }

  /// \brief Determine if this GenericAPInt just has one word to store value.
  ///
  /// \returns true if the number of bits <= 64, false otherwise.
  bool isSingleWord() const { return APIntRef(*this).isSingleWord(); }

  /// \brief Compute the number of active bits in the value
  ///
  /// This function returns the number of active bits which is defined as the
  /// bit width minus the number of leading zeros. This is used in several
  /// computations to see how "wide" the value is.
  unsigned getActiveBits() const { return APIntRef(*this).getActiveBits(); }

  MutableArrayRef<uint64_t> words() const {
    if (!needsCleanup())
      return {InlineStorage, getNumWords()};
    return {OutOfLineStorage, getNumWords()};
  }

  /// \brief Clear unused high order bits
  ///
  /// This method is used internally to clear the top "N" bits in the high order
  /// word that are not used by the GenericAPInt. This is needed after the most
  /// significant word is assigned a value to ensure that those bits are
  /// zero'd out.
  void clearUnusedBits() {
    // Compute how many bits are used in the final word
    unsigned wordBits = BitWidth % APINT_BITS_PER_WORD;
    if (wordBits == 0)
      // If all bits are used, we want to leave the value alone. This also
      // avoids the undefined behavior of >> when the shift is the same size as
      // the word size (64).
      return;

    // Mask out the high bits.
    uint64_t mask = ~uint64_t(0ULL) >> (APINT_BITS_PER_WORD - wordBits);
    words().back() &= mask;
  }

  /// \brief Set every bit to 0.
  void clearAllBits() {
    if (isSingleWord()) {
      words().back() = 0;
    } else{
      for (uint64_t &Word : words())
        Word = 0;
    }
  }

  /// @brief Prefix increment operator. Increments the APInt by one.
  void increment();

  /// @brief Prefix decrement operator. Decrements the APInt by one.
  void decrement();

  /// Adds the RHS APint to this APInt.
  /// @brief Addition assignment operator.
  void add(APIntRef RHS);

  /// Adds the RHS uint64_t to this APInt.
  /// @brief Addition assignment operator.
  void add(uint64_t RHS);

  /// Subtract the RHS APint to this APInt.
  /// @brief Subtraction assignment operator.
  void subtract(APIntRef RHS);

  /// Subtract the RHS uint64_t to this APInt.
  /// @brief Subtraction assignment operator.
  void subtract(uint64_t RHS);

  /// Multiply the RHS APint to this APInt.
  /// @brief multiplication assignment operator.
  void multiply(APIntRef RHS);

  /// Divide the RHS APint to this APInt.
  /// @brief division assignment operator.
  void udiv(APIntRef RHS);

  /// Modulo the RHS APint to this APInt.
  /// @brief remainder assignment operator.
  void urem(APIntRef RHS);

  /// \brief An internal division function for dividing APIntRefs.
  ///
  /// This is used by the toString method to divide by the radix. It simply
  /// provides a more convenient form of divide for internal use since KnuthDiv
  /// has specific constraints on its inputs. If those constraints are not met
  /// then it provides a simpler form of divide.
  static void divide(APIntRef LHS, unsigned lhsWords,
                     APIntRef RHS, unsigned rhsWords,
                     APIntImpl *Quotient, APIntImpl *Remainder);

  /// \brief Dual division/remainder interface.
  ///
  /// Sometimes it is convenient to divide two GenericAPInt values and obtain both the
  /// quotient and remainder. This function does both operations in the same
  /// computation making it a little more efficient. The pair of input arguments
  /// may overlap with the pair of output arguments. It is safe to call
  /// udivrem(X, Y, X, Y), for example.
  static void udivrem(APIntRef LHS, APIntRef RHS,
                      APIntImpl Quotient, APIntImpl Remainder);

  /// \brief Convert a char array into a GenericAPInt
  ///
  /// \param radix 2, 8, 10, 16, or 36
  /// Converts a string into a number.  The string must be non-empty
  /// and well-formed as a number of the given base. The bit-width
  /// must be sufficient to hold the result.
  ///
  /// This is used by the constructors that take string arguments.
  ///
  /// StringRef::getAsInteger is superficially similar but (1) does
  /// not assume that the string is well-formed and (2) grows the
  /// result to hold the input.
  void fromString(unsigned numBits, StringRef str, uint8_t radix);

  /// And the RHS APint to this APInt.
  /// @brief and assignment operator.
  void and_op(APIntRef RHS);

  /// Or the RHS APint to this APInt.
  /// @brief or assignment operator.
  void or_op(APIntRef RHS);

  /// Xor the RHS APint to this APInt.
  /// @brief xor assignment operator.
  void xor_op(APIntRef RHS);

  /// Left shift this APInt by the RHS uint64_t.
  /// @brief left shift assignment operator.
  void shl(unsigned shiftAmt);

  /// Arithmetic right shift this APInt by the RHS uint64_t.
  /// @brief arithmetic right shift assignment operator.
  void ashr(unsigned shiftAmt);

  /// Logical right shift this APInt by the RHS uint64_t.
  /// @brief logical right shift assignment operator.
  void lshr(unsigned shiftAmt);

  /// \returns the multiplicative inverse for a given modulo.
  void multiplicativeInverse(APIntRef modulo);

  /// out-of-line slow case for operator=
  void assignSlowCase(APIntRef RHS);

  /// \brief Set a given bit to 1.
  ///
  /// Set the given bit to 1 whose position is given as "bitPosition".
  void setBit(unsigned bitPosition) {
    assert(bitPosition < BitWidth && "Out of the bit-width range!");
    words()[APIntRef::whichWord(bitPosition)] |= APIntRef::maskBit(bitPosition);
  }

  /// \brief Set a given bit to 0.
  ///
  /// Set the given bit to 0 whose position is given as "bitPosition".
  void clearBit(unsigned bitPosition) {
    assert(bitPosition < BitWidth && "Out of the bit-width range!");
    words()[APIntRef::whichWord(bitPosition)] &=
        ~APIntRef::maskBit(bitPosition);
  }

  /// \brief Toggle every bit to its opposite value.
  void flipAllBits() {
    for (uint64_t &Word : words())
      Word ^= UINT64_MAX;
    clearUnusedBits();
  }

  /// Toggle a given bit to its opposite value whose position is given
  /// as "bitPosition".
  /// @brief Toggles a given bit to its opposite value.
  void flipBit(unsigned bitPosition) {
    assert(bitPosition < BitWidth && "Out of the bit-width range!");
    if (APIntRef(*this)[bitPosition])
      clearBit(bitPosition);
    else
      setBit(bitPosition);
  }

  /// \brief Determine sign of this GenericAPInt.
  ///
  /// This tests the high bit of this GenericAPInt to determine if it is set.
  ///
  /// \returns true if this GenericAPInt is negative, false otherwise
  bool isNegative() const { return APIntRef(*this).isNegative(); }

  // Performs a byte swap.
  void byteSwap();

  // Reverses the bits of this value.
  void reverseBits();

  /// \brief Truncate to new width.
  ///
  /// Truncate the APIntImpl to a specified width. It is an error to specify a
  /// width that is greater than or equal to the current width.
  void trunc(unsigned width);

  /// \brief Sign extend to a new width.
  ///
  /// This operation sign extends the APIntImpl to a new width. If the high order
  /// bit is set, the fill on the left will be done with 1 bits, otherwise zero.
  /// It is an error to specify a width that is less than or equal to the
  /// current width.
  void sext(unsigned width);

  /// \brief Zero extend to a new width.
  ///
  /// This operation zero extends the APIntImpl to a new width. The high order bits
  /// are filled with 0 bits.  It is an error to specify a width that is less
  /// than or equal to the current width.
  void zext(unsigned width);

  uint64_t *InlineStorage;
  uint64_t *&OutOfLineStorage;
  unsigned &BitWidth;
  const unsigned NumInlineWords;
};

inline APIntRef::APIntRef(const APIntImpl &Impl)
    : Words(Impl.words()), BitWidth(Impl.getBitWidth()) {}

//===----------------------------------------------------------------------===//
//                              GenericAPInt Class
//===----------------------------------------------------------------------===//

/// \brief Class for arbitrary precision integers.
///
/// GenericAPInt is a functional replacement for common case unsigned integer
/// type like "unsigned", "unsigned long" or "uint64_t", but also allows
/// non-byte-width integer sizes and large integer value types such as 3-bits,
/// 15-bits, or more than 64-bits of precision. GenericAPInt provides a variety
/// of arithmetic operators and methods to manipulate integer values of any
/// bit-width. It supports both the typical integer arithmetic and comparison
/// operations as well as bitwise manipulation.
///
/// The class has several invariants worth noting:
///   * All bit, byte, and word positions are zero-based.
///   * Once the bit width is set, it doesn't change except by the Truncate,
///     SignExtend, or ZeroExtend operations.
///   * All binary operators must be on GenericAPInt instances of the same bit
///     width.
///     Attempting to use these operators on instances with different bit
///     widths will yield an assertion.
///   * The value is stored canonically as an unsigned value. For operations
///     where it makes a difference, there are both signed and unsigned variants
///     of the operation. For example, sdiv and udiv. However, because the bit
///     widths must be the same, operations such as Mul and Add produce the same
///     results regardless of whether the values are interpreted as signed or
///     not.
///   * In general, the class tries to follow the style of computation that LLVM
///     uses in its IR. This simplifies its use for LLVM.
///
template <unsigned NumInlineWords>
class GenericAPInt {
  unsigned BitWidth; ///< The number of bits in this GenericAPInt.

  enum { NumInlineBits = NumInlineWords * APINT_BITS_PER_WORD };

  /// This union is used to store the integer value. When the
  /// integer bit-width <= NumInlineBits, it uses VAL, otherwise it uses pVal.
  union {
    uint64_t VAL[NumInlineWords]; ///< Used to store the <= NumInlineBits bits
                                  ///< integer value.
    uint64_t *pVal; ///< Used to store the >NumInlineBits bits integer value.
  };

  friend struct DenseMapAPIntKeyInfo;

  /// \brief Fast internal constructor
  ///
  /// This constructor is used only internally for speed of construction of
  /// temporaries. It is unsafe for general use so it is not public.
  GenericAPInt(uint64_t *val, unsigned bits) : BitWidth(bits), pVal(val) {}

  // Simply makes *this a copy of Ref.
  GenericAPInt(APIntRef Ref) : BitWidth(Ref.getBitWidth()) {
    if (isSingleWord())
      VAL[0] = Ref.words()[0];
    else
      APIntImpl(*this).initSlowCase(Ref);
    clearUnusedBits();
  }

  /// \brief Determine if this GenericAPInt just has one word to store value.
  ///
  /// \returns true if the number of bits <= 64, false otherwise.
  bool isSingleWord() const { return BitWidth <= APINT_BITS_PER_WORD; }

  MutableArrayRef<uint64_t> words() const {
    if (!needsCleanup())
      return {&VAL[0], &VAL[getNumWords()]};
    return MutableArrayRef<uint64_t>(pVal, getNumWords());
  }

  /// \brief Clear unused high order bits
  ///
  /// This method is used internally to clear the top "N" bits in the high order
  /// word that are not used by the GenericAPInt. This is needed after the most
  /// significant word is assigned a value to ensure that those bits are
  /// zero'd out.
  GenericAPInt &clearUnusedBits() {
    APIntImpl(*this).clearUnusedBits();
    return *this;
  }

  /// \brief Get the requested word.
  /// \returns the corresponding word for the specified bit position.
  uint64_t getWord(unsigned Word) const { return words()[Word]; }

  /// \brief Convert a char array into a GenericAPInt
  ///
  /// \param radix 2, 8, 10, 16, or 36
  /// Converts a string into a number.  The string must be non-empty
  /// and well-formed as a number of the given base. The bit-width
  /// must be sufficient to hold the result.
  ///
  /// This is used by the constructors that take string arguments.
  ///
  /// StringRef::getAsInteger is superficially similar but (1) does
  /// not assume that the string is well-formed and (2) grows the
  /// result to hold the input.
  void fromString(unsigned numBits, StringRef str, uint8_t radix) {
    APIntImpl(*this).fromString(numBits, str, radix);
  }

public:
  /// \name Constructors
  /// @{

  /// \brief Create a new GenericAPInt of numBits width, initialized as val.
  ///
  /// If isSigned is true then val is treated as if it were a signed value
  /// (i.e. as an int64_t) and the appropriate sign extension to the bit width
  /// will be done. Otherwise, no sign extension occurs (high order bits beyond
  /// the range of val are zero filled).
  ///
  /// \param numBits the bit width of the constructed GenericAPInt
  /// \param val the initial value of the GenericAPInt
  /// \param isSigned how to treat signedness of val
  GenericAPInt(unsigned numBits, uint64_t val, bool isSigned = false)
      : BitWidth(numBits) {
    assert(BitWidth && "bitwidth too small");
    if (isSingleWord())
      VAL[0] = val;
    else
      APIntImpl(*this).initSlowCase(val, isSigned);
    clearUnusedBits();
  }

  /// \brief Construct a GenericAPInt of numBits width, initialized as bigVal[].
  ///
  /// Note that bigVal.size() can be smaller or larger than the corresponding
  /// bit width but any extraneous bits will be dropped.
  ///
  /// \param numBits the bit width of the constructed GenericAPInt
  /// \param bigVal a sequence of words to form the initial value of the
  /// GenericAPInt
  GenericAPInt(unsigned NumBits, ArrayRef<uint64_t> BigVal)
      : BitWidth(NumBits) {
    APIntImpl(*this).initFromArray(BigVal);
  }

  /// Equivalent to GenericAPInt(numBits, ArrayRef<uint64_t>(bigVal, numWords)),
  /// but deprecated because this constructor is prone to ambiguity with the
  /// GenericAPInt(unsigned, uint64_t, bool) constructor.
  ///
  /// If this overload is ever deleted, care should be taken to prevent calls
  /// from being incorrectly captured by the GenericAPInt(unsigned, uint64_t,
  /// bool) constructor.
  GenericAPInt(unsigned NumBits, unsigned NumWords, const uint64_t BigVal[])
      : BitWidth(NumBits) {
    APIntImpl(*this).initFromArray({BigVal, NumWords});
  }

  /// \brief Construct a GenericAPInt from a string representation.
  ///
  /// This constructor interprets the string \p str in the given radix. The
  /// interpretation stops when the first character that is not suitable for the
  /// radix is encountered, or the end of the string. Acceptable radix values
  /// are 2, 8, 10, 16, and 36. It is an error for the value implied by the
  /// string to require more bits than numBits.
  ///
  /// \param numBits the bit width of the constructed GenericAPInt
  /// \param str the string to be interpreted
  /// \param radix the radix to use for the conversion
  GenericAPInt(unsigned NumBits, StringRef Str, uint8_t Radix)
      : BitWidth(NumBits) {
    assert(BitWidth && "Bitwidth too small");
    fromString(NumBits, Str, Radix);
  }

  /// Simply makes *this a copy of that.
  /// @brief Copy Constructor.
  GenericAPInt(const GenericAPInt &that) : BitWidth(that.BitWidth) {
    if (isSingleWord())
      VAL[0] = that.VAL[0];
    else
      APIntImpl(*this).initSlowCase(that);
  }

  /// \brief Move Constructor.
  GenericAPInt(GenericAPInt &&that) : BitWidth(that.BitWidth) {
    if (needsCleanup())
      pVal = that.pVal;
    else
      std::copy(that.words().begin(), that.words().end(), words().begin());
    that.BitWidth = 0;
  }

  /// \brief Destructor.
  ~GenericAPInt() {
    if (needsCleanup())
      delete[] pVal;
  }

  /// \brief Default constructor that creates an uninteresting GenericAPInt
  /// representing a 1-bit zero value.
  ///
  /// This is useful for object deserialization (pair this with the static
  ///  method Read).
  explicit GenericAPInt() : BitWidth(1) { VAL[0] = 0; }

  /// \brief Returns whether this instance allocated memory.
  bool needsCleanup() const { return getBitWidth() > NumInlineBits; }

  /// Used to insert GenericAPInt objects, or objects that contain GenericAPInt
  ///  objects, into FoldingSets.
  void Profile(FoldingSetNodeID &ID) const {
    APIntRef ThisRef(*this);
    ThisRef.Profile(ID);
  }

  /// @}
  /// \name Value Tests
  /// @{

  /// \brief Determine sign of this GenericAPInt.
  ///
  /// This tests the high bit of this GenericAPInt to determine if it is set.
  ///
  /// \returns true if this GenericAPInt is negative, false otherwise
  bool isNegative() const { return APIntRef(*this).isNegative(); }

  /// \brief Determine if this GenericAPInt Value is non-negative (>= 0)
  ///
  /// This tests the high bit of the GenericAPInt to determine if it is unset.
  bool isNonNegative() const { return APIntRef(*this).isNonNegative(); }

  /// \brief Determine if this GenericAPInt Value is positive.
  ///
  /// This tests if the value of this GenericAPInt is positive (> 0). Note
  /// that 0 is not a positive value.
  ///
  /// \returns true if this GenericAPInt is positive.
  bool isStrictlyPositive() const { return isNonNegative() && !!*this; }

  /// \brief Determine if all bits are set
  ///
  /// This checks to see if the value has all bits of the GenericAPInt are set
  /// or not.
  bool isAllOnesValue() const {
    if (isSingleWord())
      return VAL[0] == ~integerPart(0) >> (APINT_BITS_PER_WORD - BitWidth);
    return APIntRef(*this).countPopulationSlowCase() == BitWidth;
  }

  /// \brief Determine if this is the largest unsigned value.
  ///
  /// This checks to see if the value of this GenericAPInt is the maximum unsigned
  /// value for the GenericAPInt's bit width.
  bool isMaxValue() const { return isAllOnesValue(); }

  /// \brief Determine if this is the largest signed value.
  ///
  /// This checks to see if the value of this GenericAPInt is the maximum signed
  /// value for the GenericAPInt's bit width.
  bool isMaxSignedValue() const {
    return !isNegative() && countPopulation() == BitWidth - 1;
  }

  /// \brief Determine if this is the smallest unsigned value.
  ///
  /// This checks to see if the value of this GenericAPInt is the minimum unsigned
  /// value for the GenericAPInt's bit width.
  bool isMinValue() const { return !*this; }

  /// \brief Determine if this is the smallest signed value.
  ///
  /// This checks to see if the value of this GenericAPInt is the minimum signed
  /// value for the GenericAPInt's bit width.
  bool isMinSignedValue() const {
    return isNegative() && isPowerOf2();
  }

  /// \brief Check if this GenericAPInt has an N-bits unsigned integer value.
  bool isIntN(unsigned N) const {
    assert(N && "N == 0 ???");
    return getActiveBits() <= N;
  }

  /// \brief Check if this GenericAPInt has an N-bits signed integer value.
  bool isSignedIntN(unsigned N) const {
    assert(N && "N == 0 ???");
    return getMinSignedBits() <= N;
  }

  /// \brief Check if this GenericAPInt's value is a power of two greater than zero.
  ///
  /// \returns true if the argument GenericAPInt value is a power of two > 0.
  bool isPowerOf2() const {
    if (isSingleWord())
      return isPowerOf2_64(VAL[0]);
    return APIntRef(*this).countPopulationSlowCase() == 1;
  }

  /// \brief Check if the GenericAPInt's value is returned by getSignBit.
  ///
  /// \returns true if this is the value returned by getSignBit.
  bool isSignBit() const { return isMinSignedValue(); }

  /// \brief Convert GenericAPInt to a boolean value.
  ///
  /// This converts the GenericAPInt to a boolean value as a test against zero.
  bool getBoolValue() const { return !!*this; }

  /// If this value is smaller than the specified limit, return it, otherwise
  /// return the limit value.  This causes the value to saturate to the limit.
  uint64_t getLimitedValue(uint64_t Limit = ~0ULL) const {
    return (getActiveBits() > 64 || getZExtValue() > Limit) ? Limit
                                                            : getZExtValue();
  }

  /// \brief Check if the GenericAPInt consists of a repeated bit pattern.
  ///
  /// e.g. 0x01010101 satisfies isSplat(8).
  /// \param SplatSizeInBits The size of the pattern in bits. Must divide bit
  /// width without remainder.
  bool isSplat(unsigned SplatSizeInBits) const {
    assert(getBitWidth() % SplatSizeInBits == 0 &&
           "SplatSizeInBits must divide width!");
    // We can check that all parts of an integer are equal by making use of a
    // little trick: rotate and check if it's still the same value.
    return *this == rotl(SplatSizeInBits);
  }

  /// @}
  /// \name Value Generators
  /// @{

  /// \brief Gets maximum unsigned value of GenericAPInt for specific bit width.
  static GenericAPInt getMaxValue(unsigned numBits) {
    return getAllOnesValue(numBits);
  }

  /// \brief Gets maximum signed value of GenericAPInt for a specific bit width.
  static GenericAPInt getSignedMaxValue(unsigned numBits) {
    GenericAPInt API = getAllOnesValue(numBits);
    API.clearBit(numBits - 1);
    return API;
  }

  /// \brief Gets minimum unsigned value of GenericAPInt for a specific bit width.
  static GenericAPInt getMinValue(unsigned numBits) { return GenericAPInt(numBits, 0); }

  /// \brief Gets minimum signed value of GenericAPInt for a specific bit width.
  static GenericAPInt getSignedMinValue(unsigned numBits) {
    GenericAPInt API(numBits, 0);
    API.setBit(numBits - 1);
    return API;
  }

  /// \brief Get the SignBit for a specific bit width.
  ///
  /// This is just a wrapper function of getSignedMinValue(), and it helps code
  /// readability when we want to get a SignBit.
  static GenericAPInt getSignBit(unsigned BitWidth) {
    return getSignedMinValue(BitWidth);
  }

  /// \brief Get the all-ones value.
  ///
  /// \returns the all-ones value for an GenericAPInt of the specified bit-width.
  static GenericAPInt getAllOnesValue(unsigned numBits) {
    return GenericAPInt(numBits, UINT64_MAX, true);
  }

  /// \brief Get the '0' value.
  ///
  /// \returns the '0' value for an GenericAPInt of the specified bit-width.
  static GenericAPInt getNullValue(unsigned numBits) { return GenericAPInt(numBits, 0); }

  /// \brief Compute an GenericAPInt containing numBits highbits from this GenericAPInt.
  ///
  /// Get an GenericAPInt with the same BitWidth as this GenericAPInt, just zero mask
  /// the low bits and right shift to the least significant bit.
  ///
  /// \returns the high "numBits" bits of this GenericAPInt.
  GenericAPInt getHiBits(unsigned numBits) const {
    return this->lshr(BitWidth - numBits);
  }

  /// \brief Compute an GenericAPInt containing numBits lowbits from this GenericAPInt.
  ///
  /// Get an GenericAPInt with the same BitWidth as this GenericAPInt, just zero mask
  /// the high bits.
  ///
  /// \returns the low "numBits" bits of this GenericAPInt.
  GenericAPInt getLoBits(unsigned numBits) const {
    return this->shl(BitWidth - numBits).lshr(BitWidth - numBits);
  }

  /// \brief Return an GenericAPInt with exactly one bit set in the result.
  static GenericAPInt getOneBitSet(unsigned numBits, unsigned BitNo) {
    GenericAPInt Res(numBits, 0);
    Res.setBit(BitNo);
    return Res;
  }

  /// \brief Get a value with a block of bits set.
  ///
  /// Constructs an GenericAPInt value that has a contiguous range of bits set. The
  /// bits from loBit (inclusive) to hiBit (exclusive) will be set. All other
  /// bits will be zero. For example, with parameters(32, 0, 16) you would get
  /// 0x0000FFFF. If hiBit is less than loBit then the set bits "wrap". For
  /// example, with parameters (32, 28, 4), you would get 0xF000000F.
  ///
  /// \param numBits the intended bit width of the result
  /// \param loBit the index of the lowest bit set.
  /// \param hiBit the index of the highest bit set.
  ///
  /// \returns An GenericAPInt value with the requested bits set.
  static GenericAPInt getBitsSet(unsigned numBits, unsigned loBit, unsigned hiBit) {
    assert(hiBit <= numBits && "hiBit out of range");
    assert(loBit < numBits && "loBit out of range");
    if (hiBit < loBit)
      return getLowBitsSet(numBits, hiBit) |
             getHighBitsSet(numBits, numBits - loBit);
    return getLowBitsSet(numBits, hiBit - loBit).shl(loBit);
  }

  /// \brief Get a value with high bits set
  ///
  /// Constructs an GenericAPInt value that has the top hiBitsSet bits set.
  ///
  /// \param numBits the bitwidth of the result
  /// \param hiBitsSet the number of high-order bits set in the result.
  static GenericAPInt getHighBitsSet(unsigned numBits, unsigned hiBitsSet) {
    assert(hiBitsSet <= numBits && "Too many bits to set!");
    // Handle a degenerate case, to avoid shifting by word size
    if (hiBitsSet == 0)
      return GenericAPInt(numBits, 0);
    unsigned shiftAmt = numBits - hiBitsSet;
    // For small values, return quickly
    if (numBits <= APINT_BITS_PER_WORD)
      return GenericAPInt(numBits, ~0ULL << shiftAmt);
    return getAllOnesValue(numBits).shl(shiftAmt);
  }

  /// \brief Get a value with low bits set
  ///
  /// Constructs an GenericAPInt value that has the bottom loBitsSet bits set.
  ///
  /// \param numBits the bitwidth of the result
  /// \param loBitsSet the number of low-order bits set in the result.
  static GenericAPInt getLowBitsSet(unsigned numBits, unsigned loBitsSet) {
    assert(loBitsSet <= numBits && "Too many bits to set!");
    // Handle a degenerate case, to avoid shifting by word size
    if (loBitsSet == 0)
      return GenericAPInt(numBits, 0);
    if (loBitsSet == APINT_BITS_PER_WORD)
      return GenericAPInt(numBits, UINT64_MAX);
    // For small values, return quickly.
    if (loBitsSet <= APINT_BITS_PER_WORD)
      return GenericAPInt(numBits, UINT64_MAX >> (APINT_BITS_PER_WORD - loBitsSet));
    return getAllOnesValue(numBits).lshr(numBits - loBitsSet);
  }

  /// \brief Return a value containing V broadcasted over NewLen bits.
  static GenericAPInt getSplat(unsigned NewLen, const GenericAPInt &V) {
    assert(NewLen >= V.getBitWidth() && "Can't splat to smaller bit width!");

    GenericAPInt Val = V.zextOrSelf(NewLen);
    for (unsigned I = V.getBitWidth(); I < NewLen; I <<= 1)
      Val |= Val << I;

    return Val;
  }

  /// \brief Determine if two GenericAPInts have the same value, after zero-extending
  /// one of them (if needed!) to ensure that the bit-widths match.
  static bool isSameValue(const GenericAPInt &I1, const GenericAPInt &I2) {
    if (I1.getBitWidth() == I2.getBitWidth())
      return I1 == I2;

    if (I1.getBitWidth() > I2.getBitWidth())
      return I1 == I2.zext(I1.getBitWidth());

    return I1.zext(I2.getBitWidth()) == I2;
  }

  /// \brief Overload to compute a hash_code for an GenericAPInt value.
  friend hash_code hash_value(const GenericAPInt &Arg) {
    return hash_value(APIntRef(Arg));
  }

  friend class APIntImpl;
  friend class APIntRef;

  /// This function returns a pointer to the internal storage of the GenericAPInt.
  /// This is useful for writing out the GenericAPInt in binary form without any
  /// conversions.
  const uint64_t *getRawData() const {
    return &words().front();
  }

  /// @}
  /// \name Unary Operators
  /// @{

  /// \brief Postfix increment operator.
  ///
  /// \returns a new GenericAPInt value representing *this incremented by one
  const GenericAPInt operator++(int) {
    GenericAPInt API(*this);
    ++(*this);
    return API;
  }

  /// \brief Prefix increment operator.
  ///
  /// \returns *this incremented by one
  GenericAPInt &operator++() {
    APIntImpl(*this).increment();
    return *this;
  }

  /// \brief Postfix decrement operator.
  ///
  /// \returns a new GenericAPInt representing *this decremented by one.
  const GenericAPInt operator--(int) {
    GenericAPInt API(*this);
    --(*this);
    return API;
  }

  /// \brief Prefix decrement operator.
  ///
  /// \returns *this decremented by one.
  GenericAPInt &operator--() {
    APIntImpl(*this).decrement();
    return *this;
  }

  /// \brief Unary bitwise complement operator.
  ///
  /// Performs a bitwise complement operation on this GenericAPInt.
  ///
  /// \returns an GenericAPInt that is the bitwise complement of *this
  GenericAPInt operator~() const {
    GenericAPInt Result(*this);
    Result.flipAllBits();
    return Result;
  }

  /// \brief Logical negation operator.
  ///
  /// Performs logical negation operation on this GenericAPInt.
  ///
  /// \returns true if *this is zero, false otherwise.
  bool operator!() const {
    for (uint64_t Word : words())
      if (Word)
        return false;
    return true;
  }

  /// @}
  /// \name Assignment Operators
  /// @{

  /// \brief Copy assignment operator.
  ///
  /// \returns *this after assignment of RHS.
  GenericAPInt &operator=(const GenericAPInt &RHS) {
    // If the bitwidths are the same, we can avoid mucking with memory
    if (isSingleWord() && RHS.isSingleWord()) {
      VAL[0] = RHS.VAL[0];
      BitWidth = RHS.BitWidth;
      return clearUnusedBits();
    }

    // Don't do anything for X = X
    if (this == &RHS)
      return *this;

    APIntImpl(*this).assignSlowCase(RHS);
    return *this;
  }

  /// @brief Move assignment operator.
  GenericAPInt &operator=(GenericAPInt &&that) {
    // The MSVC STL shipped in 2013 requires that self move assignment be a
    // no-op.  Otherwise algorithms like stable_sort will produce answers
    // where half of the output is left in a moved-from state.
    if (this == &that)
      return *this;

    if (needsCleanup())
      delete[] pVal;
    BitWidth = that.BitWidth;
    if (that.needsCleanup()) {
      pVal = that.pVal;
    } else {
      std::copy(that.words().begin(), that.words().end(), words().begin());
    }
    that.BitWidth = 0;

    return *this;
  }

  /// \brief Assignment operator.
  ///
  /// The RHS value is assigned to *this. If the significant bits in RHS exceed
  /// the bit width, the excess bits are truncated. If the bit width is larger
  /// than 64, the value is zero filled in the unspecified high order bits.
  ///
  /// \returns *this after assignment of RHS value.
  GenericAPInt &operator=(uint64_t RHS) {
    clearAllBits();
    words().front() = RHS;
    return clearUnusedBits();
  }

  /// \brief Bitwise AND assignment operator.
  ///
  /// Performs a bitwise AND operation on this GenericAPInt and RHS. The result is
  /// assigned to *this.
  ///
  /// \returns *this after ANDing with RHS.
  GenericAPInt &operator&=(const GenericAPInt &RHS) {
    APIntImpl(*this).and_op(RHS);
    return *this;
  }

  /// \brief Bitwise OR assignment operator.
  ///
  /// Performs a bitwise OR operation on this GenericAPInt and RHS. The result is
  /// assigned *this;
  ///
  /// \returns *this after ORing with RHS.
  GenericAPInt &operator|=(const GenericAPInt &RHS) {
    APIntImpl(*this).or_op(RHS);
    return *this;
  }

  /// \brief Bitwise OR assignment operator.
  ///
  /// Performs a bitwise OR operation on this GenericAPInt and RHS. RHS is
  /// logically zero-extended or truncated to match the bit-width of
  /// the LHS.
  GenericAPInt &operator|=(uint64_t RHS) {
    words()[0] |= RHS;
    clearUnusedBits();
    return *this;
  }

  /// \brief Bitwise XOR assignment operator.
  ///
  /// Performs a bitwise XOR operation on this GenericAPInt and RHS. The result is
  /// assigned to *this.
  ///
  /// \returns *this after XORing with RHS.
  GenericAPInt &operator^=(const GenericAPInt &RHS) {
    APIntImpl(*this).xor_op(RHS);
    return *this;
  }

  /// \brief Multiplication assignment operator.
  ///
  /// Multiplies this GenericAPInt by RHS and assigns the result to *this.
  ///
  /// \returns *this
  GenericAPInt &operator*=(const GenericAPInt &RHS) {
    APIntImpl(*this).multiply(RHS);
    return *this;
  }

  /// \brief Addition assignment operator.
  ///
  /// Adds RHS to *this and assigns the result to *this.
  ///
  /// \returns *this
  GenericAPInt &operator+=(const GenericAPInt &RHS) {
    APIntImpl(*this).add(RHS);
    return *this;
  }
  GenericAPInt &operator+=(uint64_t RHS) {
    APIntImpl(*this).add(RHS);
    return *this;
  }

  /// \brief Subtraction assignment operator.
  ///
  /// Subtracts RHS from *this and assigns the result to *this.
  ///
  /// \returns *this
  GenericAPInt &operator-=(const GenericAPInt &RHS) {
    APIntImpl(*this).subtract(RHS);
    return *this;
  }
  GenericAPInt &operator-=(uint64_t RHS) {
    APIntImpl(*this).subtract(RHS);
    return *this;
  }

  /// \brief Left-shift assignment function.
  ///
  /// Shifts *this left by shiftAmt and assigns the result to *this.
  ///
  /// \returns *this after shifting left by shiftAmt
  GenericAPInt &operator<<=(unsigned shiftAmt) {
    APIntImpl(*this).shl(shiftAmt);
    return *this;
  }

  /// @}
  /// \name Binary Operators
  /// @{

  /// \brief Bitwise AND operator.
  ///
  /// Performs a bitwise AND operation on *this and RHS.
  ///
  /// \returns An GenericAPInt value representing the bitwise AND of *this and RHS.
  GenericAPInt operator&(const GenericAPInt &RHS) const {
    assert(BitWidth == RHS.BitWidth && "Bit widths must be the same");
    if (isSingleWord())
      return GenericAPInt(getBitWidth(), VAL[0] & RHS.VAL[0]);

    GenericAPInt Result(*this);
    Result &= RHS;
    return Result;
  }
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT And(const GenericAPInt &RHS) const {
    return this->operator&(RHS);
  }

  /// \brief Bitwise OR operator.
  ///
  /// Performs a bitwise OR operation on *this and RHS.
  ///
  /// \returns An GenericAPInt value representing the bitwise OR of *this and RHS.
  GenericAPInt operator|(const GenericAPInt &RHS) const {
    assert(BitWidth == RHS.BitWidth && "Bit widths must be the same");
    if (isSingleWord())
      return GenericAPInt(getBitWidth(), VAL[0] | RHS.VAL[0]);

    GenericAPInt Result(*this);
    Result |= RHS;
    return Result;
  }

  /// \brief Bitwise OR function.
  ///
  /// Performs a bitwise or on *this and RHS. This is implemented by simply
  /// calling operator|.
  ///
  /// \returns An GenericAPInt value representing the bitwise OR of *this and RHS.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT Or(const GenericAPInt &RHS) const {
    return this->operator|(RHS);
  }

  /// \brief Bitwise XOR operator.
  ///
  /// Performs a bitwise XOR operation on *this and RHS.
  ///
  /// \returns An GenericAPInt value representing the bitwise XOR of *this and RHS.
  GenericAPInt operator^(const GenericAPInt &RHS) const {
    assert(BitWidth == RHS.BitWidth && "Bit widths must be the same");
    if (isSingleWord())
      return GenericAPInt(BitWidth, VAL[0] ^ RHS.VAL[0]);

    GenericAPInt Result(*this);
    Result ^= RHS;
    return Result;
  }

  /// \brief Bitwise XOR function.
  ///
  /// Performs a bitwise XOR operation on *this and RHS. This is implemented
  /// through the usage of operator^.
  ///
  /// \returns An GenericAPInt value representing the bitwise XOR of *this and RHS.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT Xor(const GenericAPInt &RHS) const {
    return this->operator^(RHS);
  }

  /// \brief Multiplication operator.
  ///
  /// Multiplies this GenericAPInt by RHS and returns the result.
  GenericAPInt operator*(const GenericAPInt &RHS) const {
    GenericAPInt Result(*this);
    Result *= RHS;
    return Result;
  }

  /// \brief Left logical shift operator.
  ///
  /// Shifts this GenericAPInt left by \p Bits and returns the result.
  GenericAPInt operator<<(unsigned Bits) const { return shl(Bits); }

  /// \brief Left logical shift operator.
  ///
  /// Shifts this GenericAPInt left by \p Bits and returns the result.
  GenericAPInt operator<<(const GenericAPInt &Bits) const { return shl(Bits); }

  /// \brief Arithmetic right-shift function.
  ///
  /// Arithmetic right-shift this GenericAPInt by shiftAmt.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT ashr(unsigned shiftAmt) const {
    GenericAPInt Result(*this);
    APIntImpl(Result).ashr(shiftAmt);
    return Result;
  }

  /// \brief Logical right-shift function.
  ///
  /// Logical right-shift this GenericAPInt by shiftAmt.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT lshr(unsigned shiftAmt) const {
    GenericAPInt Result(*this);
    APIntImpl(Result).lshr(shiftAmt);
    return Result;
  }

  /// \brief Left-shift function.
  ///
  /// Left-shift this GenericAPInt by shiftAmt.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT shl(unsigned shiftAmt) const {
    assert(shiftAmt <= BitWidth && "Invalid shift amount");
    if (isSingleWord()) {
      if (shiftAmt >= BitWidth)
        return GenericAPInt(BitWidth, 0); // avoid undefined shift results
      return GenericAPInt(BitWidth, VAL[0] << shiftAmt);
    }

    GenericAPInt Result(*this);
    Result <<= shiftAmt;
    return Result;
  }

  /// \brief Rotate left by rotateAmt.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT rotl(unsigned rotateAmt) const {
    rotateAmt %= BitWidth;
    if (rotateAmt == 0)
      return *this;
    return shl(rotateAmt) | lshr(BitWidth - rotateAmt);
  }

  /// \brief Rotate right by rotateAmt.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT rotr(unsigned rotateAmt) const {
    rotateAmt %= BitWidth;
    if (rotateAmt == 0)
      return *this;
    return lshr(rotateAmt) | shl(BitWidth - rotateAmt);

  }

  /// \brief Arithmetic right-shift function.
  ///
  /// Arithmetic right-shift this GenericAPInt by shiftAmt.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT ashr(const GenericAPInt &shiftAmt) const {
    return ashr((unsigned)shiftAmt.getLimitedValue(BitWidth));
  }

  /// \brief Logical right-shift function.
  ///
  /// Logical right-shift this GenericAPInt by shiftAmt.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT lshr(const GenericAPInt &shiftAmt) const {
    return lshr((unsigned)shiftAmt.getLimitedValue(BitWidth));
  }

  /// \brief Left-shift function.
  ///
  /// Left-shift this GenericAPInt by shiftAmt.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT shl(const GenericAPInt &shiftAmt) const {
    // It's undefined behavior in C to shift by BitWidth or greater.
    return shl((unsigned)shiftAmt.getLimitedValue(BitWidth));
  }

  /// \brief Rotate left by rotateAmt.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT rotl(const GenericAPInt &rotateAmt) const {
    return rotl((unsigned)rotateAmt.getLimitedValue(BitWidth));
  }

  /// \brief Rotate right by rotateAmt.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT rotr(const GenericAPInt &rotateAmt) const {
    return rotr((unsigned)rotateAmt.getLimitedValue(BitWidth));
  }

  /// \brief Unsigned division operation.
  ///
  /// Perform an unsigned divide operation on this GenericAPInt by RHS. Both this and
  /// RHS are treated as unsigned quantities for purposes of this division.
  ///
  /// \returns a new GenericAPInt value containing the division result
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT udiv(const GenericAPInt &RHS) const {
    GenericAPInt Result(*this);
    APIntImpl(Result).udiv(RHS);
    return Result;
  }

  /// \brief Signed division function for GenericAPInt.
  ///
  /// Signed divide this GenericAPInt by GenericAPInt RHS.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT
  sdiv(const GenericAPInt &RHS) const {
    if (isNegative()) {
      if (RHS.isNegative())
        return (-(*this)).udiv(-RHS);
      return -((-(*this)).udiv(RHS));
    }
    if (RHS.isNegative())
      return -(this->udiv(-RHS));
    return this->udiv(RHS);
  }

  /// \brief Unsigned remainder operation.
  ///
  /// Perform an unsigned remainder operation on this GenericAPInt with RHS being the
  /// divisor. Both this and RHS are treated as unsigned quantities for purposes
  /// of this operation. Note that this is a true remainder operation and not a
  /// modulo operation because the sign follows the sign of the dividend which
  /// is *this.
  ///
  /// \returns a new GenericAPInt value containing the remainder result
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT
  urem(const GenericAPInt &RHS) const {
    GenericAPInt Result(*this);
    APIntImpl(Result).urem(RHS);
    return Result;
  }

  /// \brief Function for signed remainder operation.
  ///
  /// Signed remainder operation on GenericAPInt.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT
  srem(const GenericAPInt &RHS) const {
    if (isNegative()) {
      if (RHS.isNegative())
        return -((-(*this)).urem(-RHS));
      return -((-(*this)).urem(RHS));
    }
    if (RHS.isNegative())
      return this->urem(-RHS);
    return this->urem(RHS);
  }

  /// \brief Dual division/remainder interface.
  ///
  /// Sometimes it is convenient to divide two GenericAPInt values and obtain both the
  /// quotient and remainder. This function does both operations in the same
  /// computation making it a little more efficient. The pair of input arguments
  /// may overlap with the pair of output arguments. It is safe to call
  /// udivrem(X, Y, X, Y), for example.
  static void udivrem(const GenericAPInt &LHS, const GenericAPInt &RHS,
                      GenericAPInt &Quotient, GenericAPInt &Remainder) {
    APIntImpl::udivrem(LHS, RHS, Quotient, Remainder);
  }

  static void sdivrem(const GenericAPInt &LHS, const GenericAPInt &RHS, GenericAPInt &Quotient,
                      GenericAPInt &Remainder) {
  if (LHS.isNegative()) {
    if (RHS.isNegative())
      APInt::udivrem(-LHS, -RHS, Quotient, Remainder);
    else {
      APInt::udivrem(-LHS, RHS, Quotient, Remainder);
      Quotient = -Quotient;
    }
    Remainder = -Remainder;
  } else if (RHS.isNegative()) {
    APInt::udivrem(LHS, -RHS, Quotient, Remainder);
    Quotient = -Quotient;
  } else {
    APInt::udivrem(LHS, RHS, Quotient, Remainder);
  }

  }

  // Operations that return overflow indicators.
  GenericAPInt sadd_ov(const GenericAPInt &RHS, bool &Overflow) const {
    APInt Res = *this + RHS;
    Overflow = isNonNegative() == RHS.isNonNegative() &&
               Res.isNonNegative() != isNonNegative();
    return Res;
  }
  GenericAPInt uadd_ov(const GenericAPInt &RHS, bool &Overflow) const {
    APInt Res = *this + RHS;
    Overflow = Res.ult(RHS);
    return Res;
  }
  GenericAPInt ssub_ov(const GenericAPInt &RHS, bool &Overflow) const {
    APInt Res = *this - RHS;
    Overflow = isNonNegative() != RHS.isNonNegative() &&
               Res.isNonNegative() != isNonNegative();
    return Res;
  }
  GenericAPInt usub_ov(const GenericAPInt &RHS, bool &Overflow) const {
    APInt Res = *this - RHS;
    Overflow = Res.ugt(*this);
    return Res;
  }
  GenericAPInt sdiv_ov(const GenericAPInt &RHS, bool &Overflow) const {
    // MININT/-1  -->  overflow.
    Overflow = isMinSignedValue() && RHS.isAllOnesValue();
    return sdiv(RHS);
  }
  GenericAPInt smul_ov(const GenericAPInt &RHS, bool &Overflow) const {
    APInt Res = *this * RHS;

    if (*this != 0 && RHS != 0)
      Overflow = Res.sdiv(RHS) != *this || Res.sdiv(*this) != RHS;
    else
      Overflow = false;
    return Res;
  }
  GenericAPInt umul_ov(const GenericAPInt &RHS, bool &Overflow) const {
    APInt Res = *this * RHS;

    if (*this != 0 && RHS != 0)
      Overflow = Res.udiv(RHS) != *this || Res.udiv(*this) != RHS;
    else
      Overflow = false;
    return Res;
  }
  GenericAPInt sshl_ov(const GenericAPInt &ShAmt, bool &Overflow) const {
    Overflow = ShAmt.uge(getBitWidth());
    if (Overflow)
      return APInt(BitWidth, 0);

    if (isNonNegative()) // Don't allow sign change.
      Overflow = ShAmt.uge(countLeadingZeros());
    else
      Overflow = ShAmt.uge(countLeadingOnes());

    return *this << ShAmt;
  }
  GenericAPInt ushl_ov(const GenericAPInt &ShAmt, bool &Overflow) const {
    Overflow = ShAmt.uge(getBitWidth());
    if (Overflow)
      return APInt(BitWidth, 0);

    Overflow = ShAmt.ugt(countLeadingZeros());

    return *this << ShAmt;
  }

  /// \brief Array-indexing support.
  ///
  /// \returns the bit value at bitPosition
  bool operator[](unsigned bitPosition) const {
    return APIntRef(*this)[bitPosition];
  }

  /// @}
  /// \name Comparison Operators
  /// @{

  /// \brief Equality operator.
  ///
  /// Compares this GenericAPInt with RHS for the validity of the equality
  /// relationship.
  bool operator==(const GenericAPInt &RHS) const {
    return APIntRef(*this) == RHS;
  }

  /// \brief Equality operator.
  ///
  /// Compares this GenericAPInt with a uint64_t for the validity of the equality
  /// relationship.
  ///
  /// \returns true if *this == Val
  bool operator==(uint64_t Val) const { return APIntRef(*this) == Val; }

  /// \brief Equality comparison.
  ///
  /// Compares this GenericAPInt with RHS for the validity of the equality
  /// relationship.
  ///
  /// \returns true if *this == Val
  bool eq(const GenericAPInt &RHS) const { return (*this) == RHS; }

  /// \brief Inequality operator.
  ///
  /// Compares this GenericAPInt with RHS for the validity of the inequality
  /// relationship.
  ///
  /// \returns true if *this != Val
  bool operator!=(const GenericAPInt RHS) const { return !((*this) == RHS); }

  /// \brief Inequality operator.
  ///
  /// Compares this GenericAPInt with a uint64_t for the validity of the inequality
  /// relationship.
  ///
  /// \returns true if *this != Val
  bool operator!=(uint64_t Val) const { return !((*this) == Val); }

  /// \brief Inequality comparison
  ///
  /// Compares this GenericAPInt with RHS for the validity of the inequality
  /// relationship.
  ///
  /// \returns true if *this != Val
  bool ne(const GenericAPInt &RHS) const { return !((*this) == RHS); }

  /// \brief Unsigned less than comparison
  ///
  /// Regards both *this and RHS as unsigned quantities and compares them for
  /// the validity of the less-than relationship.
  ///
  /// \returns true if *this < RHS when both are considered unsigned.
  bool ult(const GenericAPInt &RHS) const { return APIntRef(*this).ult(RHS); }

  /// \brief Unsigned less than comparison
  ///
  /// Regards both *this as an unsigned quantity and compares it with RHS for
  /// the validity of the less-than relationship.
  ///
  /// \returns true if *this < RHS when considered unsigned.
  bool ult(uint64_t RHS) const {
    return getActiveBits() > 64 ? false : getZExtValue() < RHS;
  }

  /// \brief Signed less than comparison
  ///
  /// Regards both *this and RHS as signed quantities and compares them for
  /// validity of the less-than relationship.
  ///
  /// \returns true if *this < RHS when both are considered signed.
  bool slt(const GenericAPInt &RHS) const { return APIntRef(*this).slt(RHS); }

  /// \brief Signed less than comparison
  ///
  /// Regards both *this as a signed quantity and compares it with RHS for
  /// the validity of the less-than relationship.
  ///
  /// \returns true if *this < RHS when considered signed.
  bool slt(int64_t RHS) const {
    return getMinSignedBits() > 64 ? isNegative() : getSExtValue() < RHS;
  }

  /// \brief Unsigned less or equal comparison
  ///
  /// Regards both *this and RHS as unsigned quantities and compares them for
  /// validity of the less-or-equal relationship.
  ///
  /// \returns true if *this <= RHS when both are considered unsigned.
  bool ule(const GenericAPInt &RHS) const { return ult(RHS) || eq(RHS); }

  /// \brief Unsigned less or equal comparison
  ///
  /// Regards both *this as an unsigned quantity and compares it with RHS for
  /// the validity of the less-or-equal relationship.
  ///
  /// \returns true if *this <= RHS when considered unsigned.
  bool ule(uint64_t RHS) const { return !ugt(RHS); }

  /// \brief Signed less or equal comparison
  ///
  /// Regards both *this and RHS as signed quantities and compares them for
  /// validity of the less-or-equal relationship.
  ///
  /// \returns true if *this <= RHS when both are considered signed.
  bool sle(const GenericAPInt &RHS) const { return slt(RHS) || eq(RHS); }

  /// \brief Signed less or equal comparison
  ///
  /// Regards both *this as a signed quantity and compares it with RHS for the
  /// validity of the less-or-equal relationship.
  ///
  /// \returns true if *this <= RHS when considered signed.
  bool sle(uint64_t RHS) const { return !sgt(RHS); }

  /// \brief Unsigned greather than comparison
  ///
  /// Regards both *this and RHS as unsigned quantities and compares them for
  /// the validity of the greater-than relationship.
  ///
  /// \returns true if *this > RHS when both are considered unsigned.
  bool ugt(const GenericAPInt &RHS) const { return !ult(RHS) && !eq(RHS); }

  /// \brief Unsigned greater than comparison
  ///
  /// Regards both *this as an unsigned quantity and compares it with RHS for
  /// the validity of the greater-than relationship.
  ///
  /// \returns true if *this > RHS when considered unsigned.
  bool ugt(uint64_t RHS) const {
    return getActiveBits() > 64 ? true : getZExtValue() > RHS;
  }

  /// \brief Signed greather than comparison
  ///
  /// Regards both *this and RHS as signed quantities and compares them for the
  /// validity of the greater-than relationship.
  ///
  /// \returns true if *this > RHS when both are considered signed.
  bool sgt(const GenericAPInt &RHS) const { return !slt(RHS) && !eq(RHS); }

  /// \brief Signed greater than comparison
  ///
  /// Regards both *this as a signed quantity and compares it with RHS for
  /// the validity of the greater-than relationship.
  ///
  /// \returns true if *this > RHS when considered signed.
  bool sgt(int64_t RHS) const {
    return getMinSignedBits() > 64 ? !isNegative() : getSExtValue() > RHS;
  }

  /// \brief Unsigned greater or equal comparison
  ///
  /// Regards both *this and RHS as unsigned quantities and compares them for
  /// validity of the greater-or-equal relationship.
  ///
  /// \returns true if *this >= RHS when both are considered unsigned.
  bool uge(const GenericAPInt &RHS) const { return !ult(RHS); }

  /// \brief Unsigned greater or equal comparison
  ///
  /// Regards both *this as an unsigned quantity and compares it with RHS for
  /// the validity of the greater-or-equal relationship.
  ///
  /// \returns true if *this >= RHS when considered unsigned.
  bool uge(uint64_t RHS) const { return !ult(RHS); }

  /// \brief Signed greather or equal comparison
  ///
  /// Regards both *this and RHS as signed quantities and compares them for
  /// validity of the greater-or-equal relationship.
  ///
  /// \returns true if *this >= RHS when both are considered signed.
  bool sge(const GenericAPInt &RHS) const { return !slt(RHS); }

  /// \brief Signed greater or equal comparison
  ///
  /// Regards both *this as a signed quantity and compares it with RHS for
  /// the validity of the greater-or-equal relationship.
  ///
  /// \returns true if *this >= RHS when considered signed.
  bool sge(int64_t RHS) const { return !slt(RHS); }

  /// This operation tests if there are any pairs of corresponding bits
  /// between this GenericAPInt and RHS that are both set.
  bool intersects(const GenericAPInt &RHS) const { return (*this & RHS) != 0; }

  /// @}
  /// \name Resizing Operators
  /// @{

  /// \brief Truncate to new width.
  ///
  /// Truncate the GenericAPInt to a specified width. It is an error to specify a width
  /// that is greater than or equal to the current width.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT trunc(unsigned width) const {
    GenericAPInt Result(*this);
    APIntImpl(Result).trunc(width);
    return Result;
  }

  /// \brief Sign extend to a new width.
  ///
  /// This operation sign extends the GenericAPInt to a new width. If the high order
  /// bit is set, the fill on the left will be done with 1 bits, otherwise zero.
  /// It is an error to specify a width that is less than or equal to the
  /// current width.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT sext(unsigned width) const {
    GenericAPInt Result(*this);
    APIntImpl(Result).sext(width);
    return Result;
  }

  /// \brief Zero extend to a new width.
  ///
  /// This operation zero extends the GenericAPInt to a new width. The high order bits
  /// are filled with 0 bits.  It is an error to specify a width that is less
  /// than or equal to the current width.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT zext(unsigned width) const {
    GenericAPInt Result(*this);
    APIntImpl(Result).zext(width);
    return Result;
  }

  /// \brief Sign extend or truncate to width
  ///
  /// Make this GenericAPInt have the bit width given by \p width. The value is sign
  /// extended, truncated, or left alone to make it that width.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT sextOrTrunc(unsigned width) const {
    if (BitWidth < width)
      return sext(width);
    if (BitWidth > width)
      return trunc(width);
    return *this;
  }

  /// \brief Zero extend or truncate to width
  ///
  /// Make this GenericAPInt have the bit width given by \p width. The value is zero
  /// extended, truncated, or left alone to make it that width.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT zextOrTrunc(unsigned width) const {
    if (BitWidth < width)
      return zext(width);
    if (BitWidth > width)
      return trunc(width);
    return *this;
  }

  /// \brief Sign extend or truncate to width
  ///
  /// Make this GenericAPInt have the bit width given by \p width. The value is sign
  /// extended, or left alone to make it that width.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT sextOrSelf(unsigned width) const {
    if (BitWidth < width)
      return sext(width);
    return *this;
  }

  /// \brief Zero extend or truncate to width
  ///
  /// Make this GenericAPInt have the bit width given by \p width. The value is zero
  /// extended, or left alone to make it that width.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT zextOrSelf(unsigned width) const {
    if (BitWidth < width)
      return zext(width);
    return *this;
  }

  /// @}
  /// \name Bit Manipulation Operators
  /// @{

  /// \brief Set every bit to 1.
  void setAllBits() {
    // Set all the bits in all the words.
    for (uint64_t &Word : words())
      Word = UINT64_MAX;
    // Clear the unused ones
    clearUnusedBits();
  }

  /// \brief Set a given bit to 1.
  ///
  /// Set the given bit to 1 whose position is given as "bitPosition".
  void setBit(unsigned bitPosition) { APIntImpl(*this).setBit(bitPosition); }

  /// \brief Set every bit to 0.
  void clearAllBits() { APIntImpl(*this).clearAllBits(); }

  /// \brief Set a given bit to 0.
  ///
  /// Set the given bit to 0 whose position is given as "bitPosition".
  void clearBit(unsigned bitPosition) {
    APIntImpl(*this).clearBit(bitPosition);
  }

  /// \brief Toggle every bit to its opposite value.
  void flipAllBits() { APIntImpl(*this).flipAllBits(); }

  /// \brief Toggles a given bit to its opposite value.
  ///
  /// Toggle a given bit to its opposite value whose position is given
  /// as "bitPosition".
  void flipBit(unsigned bitPosition) { APIntImpl(*this).flipBit(bitPosition); }

  /// @}
  /// \name Value Characterization Functions
  /// @{

  /// \brief Return the number of bits in the GenericAPInt.
  unsigned getBitWidth() const { return BitWidth; }

  /// \brief Get the number of words.
  ///
  /// Here one word's bitwidth equals to that of uint64_t.
  ///
  /// \returns the number of words to hold the integer value of this GenericAPInt.
  unsigned getNumWords() const { return getNumWords(BitWidth); }

  /// \brief Get the number of words.
  ///
  /// *NOTE* Here one word's bitwidth equals to that of uint64_t.
  ///
  /// \returns the number of words to hold the integer value with a given bit
  /// width.
  static unsigned getNumWords(unsigned BitWidth) {
    return ((uint64_t)BitWidth + APINT_BITS_PER_WORD - 1) / APINT_BITS_PER_WORD;
  }

  /// \brief Compute the number of active bits in the value
  ///
  /// This function returns the number of active bits which is defined as the
  /// bit width minus the number of leading zeros. This is used in several
  /// computations to see how "wide" the value is.
  unsigned getActiveBits() const { return APIntRef(*this).getActiveBits(); }

  /// \brief Compute the number of active words in the value of this GenericAPInt.
  ///
  /// This is used in conjunction with getActiveData to extract the raw value of
  /// the GenericAPInt.
  unsigned getActiveWords() const {
    unsigned numActiveBits = getActiveBits();
    return numActiveBits ? APIntRef::whichWord(numActiveBits - 1) + 1 : 1;
  }

  /// \brief Get the minimum bit size for this signed GenericAPInt
  ///
  /// Computes the minimum bit width for this GenericAPInt while considering it to be a
  /// signed (and probably negative) value. If the value is not negative, this
  /// function returns the same value as getActiveBits()+1. Otherwise, it
  /// returns the smallest bit width that will retain the negative value. For
  /// example, -1 can be written as 0b1 or 0xFFFFFFFFFF. 0b1 is shorter and so
  /// for -1, this function will always return 1.
  unsigned getMinSignedBits() const {
    return APIntRef(*this).getMinSignedBits();
  }

  /// \brief Get zero extended value
  ///
  /// This method attempts to return the value of this GenericAPInt as a zero extended
  /// uint64_t. The bitwidth must be <= 64 or the value must fit within a
  /// uint64_t. Otherwise an assertion will result.
  uint64_t getZExtValue() const { return APIntRef(*this).getZExtValue(); }

  /// \brief Get sign extended value
  ///
  /// This method attempts to return the value of this GenericAPInt as a sign extended
  /// int64_t. The bit width must be <= 64 or the value must fit within an
  /// int64_t. Otherwise an assertion will result.
  int64_t getSExtValue() const { return APIntRef(*this).getSExtValue(); }

  /// \brief Get bits required for string value.
  ///
  /// This method determines how many bits are required to hold the APInt
  /// equivalent of the string given by \p str.
  static unsigned getBitsNeeded(StringRef str, uint8_t radix) {
    return APIntRef::getBitsNeeded(str, radix);
  }

  /// \brief The GenericAPInt version of the countLeadingZeros functions in
  ///   MathExtras.h.
  ///
  /// It counts the number of zeros from the most significant bit to the first
  /// one bit.
  ///
  /// \returns BitWidth if the value is zero, otherwise returns the number of
  ///   zeros from the most significant bit to the first one bits.
  unsigned countLeadingZeros() const {
    return APIntRef(*this).countLeadingZeros();
  }

  /// \brief Count the number of leading one bits.
  ///
  /// This function is an GenericAPInt version of the countLeadingOnes
  /// functions in MathExtras.h. It counts the number of ones from the most
  /// significant bit to the first zero bit.
  ///
  /// \returns 0 if the high order bit is not set, otherwise returns the number
  /// of 1 bits from the most significant to the least
  unsigned countLeadingOnes() const {
    return APIntRef(*this).countLeadingOnes();
  }

  /// Computes the number of leading bits of this GenericAPInt that are equal to its
  /// sign bit.
  unsigned getNumSignBits() const {
    return isNegative() ? countLeadingOnes() : countLeadingZeros();
  }

  /// \brief Count the number of trailing zero bits.
  ///
  /// This function is an GenericAPInt version of the countTrailingZeros
  /// functions in MathExtras.h. It counts the number of zeros from the least
  /// significant bit to the first set bit.
  ///
  /// \returns BitWidth if the value is zero, otherwise returns the number of
  /// zeros from the least significant bit to the first one bit.
  unsigned countTrailingZeros() const {
    return APIntRef(*this).countTrailingZeros();
  }

  /// \brief Count the number of trailing one bits.
  ///
  /// This function is an GenericAPInt version of the countTrailingOnes
  /// functions in MathExtras.h. It counts the number of ones from the least
  /// significant bit to the first zero bit.
  ///
  /// \returns BitWidth if the value is all ones, otherwise returns the number
  /// of ones from the least significant bit to the first zero bit.
  unsigned countTrailingOnes() const {
    if (isSingleWord())
      return llvm::countTrailingOnes(VAL[0]);
    return APIntRef(*this).countTrailingOnesSlowCase();
  }

  /// \brief Count the number of bits set.
  ///
  /// This function is an GenericAPInt version of the countPopulation functions
  /// in MathExtras.h. It counts the number of 1 bits in the GenericAPInt value.
  ///
  /// \returns 0 if the value is zero, otherwise returns the number of set bits.
  unsigned countPopulation() const {
    if (isSingleWord())
      return llvm::countPopulation(VAL[0]);
    return APIntRef(*this).countPopulationSlowCase();
  }

  /// @}
  /// \name Conversion Functions
  /// @{
  void print(raw_ostream &OS, bool isSigned) const {
    APIntRef(*this).print(OS, isSigned);
  }

  /// Converts an GenericAPInt to a string and append it to Str.  Str is commonly a
  /// SmallString.
  void toString(SmallVectorImpl<char> &Str, unsigned Radix, bool Signed,
                bool formatAsCLiteral = false) const {
    APIntRef(*this).toString(Str, Radix, Signed, formatAsCLiteral);
  }

  /// Considers the GenericAPInt to be unsigned and converts it into a string in the
  /// radix given. The radix can be 2, 8, 10 16, or 36.
  void toStringUnsigned(SmallVectorImpl<char> &Str, unsigned Radix = 10) const {
    toString(Str, Radix, false, false);
  }

  /// Considers the GenericAPInt to be signed and converts it into a string in the
  /// radix given. The radix can be 2, 8, 10, 16, or 36.
  void toStringSigned(SmallVectorImpl<char> &Str, unsigned Radix = 10) const {
    toString(Str, Radix, true, false);
  }

  /// \brief Return the GenericAPInt as a std::string.
  ///
  /// Note that this is an inefficient method.  It is better to pass in a
  /// SmallVector/SmallString to the methods above to avoid thrashing the heap
  /// for the string.
  std::string toString(unsigned Radix, bool Signed) const {
    return APIntRef(*this).toString(Radix, Signed);
  }

  /// \returns a byte-swapped representation of this GenericAPInt Value.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT byteSwap() const {
    GenericAPInt Result(*this);
    APIntImpl(Result).byteSwap();
    return Result;
  }

  /// \returns the value with the bit representation reversed of this GenericAPInt
  /// Value.
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT reverseBits() const {
    GenericAPInt Result(*this);
    APIntImpl(Result).reverseBits();
    return Result;
  }

  /// \brief Converts this GenericAPInt to a double value.
  double roundToDouble(bool isSigned) const {
    return APIntRef(*this).roundToDouble(isSigned);
  }

  /// \brief Converts this unsigned GenericAPInt to a double value.
  double roundToDouble() const { return roundToDouble(false); }

  /// \brief Converts this signed GenericAPInt to a double value.
  double signedRoundToDouble() const { return roundToDouble(true); }

  /// \brief Converts GenericAPInt bits to a double
  ///
  /// The conversion does not do a translation from integer to double, it just
  /// re-interprets the bits as a double. Note that it is valid to do this on
  /// any bit width. Exactly 64 bits will be translated.
  double bitsToDouble() const {
    union {
      uint64_t I;
      double D;
    } T;
    T.I = (isSingleWord() ? VAL[0] : pVal[0]);
    return T.D;
  }

  /// \brief Converts GenericAPInt bits to a double
  ///
  /// The conversion does not do a translation from integer to float, it just
  /// re-interprets the bits as a float. Note that it is valid to do this on
  /// any bit width. Exactly 32 bits will be translated.
  float bitsToFloat() const {
    union {
      unsigned I;
      float F;
    } T;
    T.I = unsigned((isSingleWord() ? VAL[0] : pVal[0]));
    return T.F;
  }

  /// \brief Converts a double to GenericAPInt bits.
  ///
  /// The conversion does not do a translation from double to integer, it just
  /// re-interprets the bits of the double.
  static GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT doubleToBits(double V) {
    union {
      uint64_t I;
      double D;
    } T;
    T.D = V;
    return GenericAPInt(sizeof T * CHAR_BIT, T.I);
  }

  /// \brief Converts a float to GenericAPInt bits.
  ///
  /// The conversion does not do a translation from float to integer, it just
  /// re-interprets the bits of the float.
  static GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT floatToBits(float V) {
    union {
      unsigned I;
      float F;
    } T;
    T.F = V;
    return GenericAPInt(sizeof T * CHAR_BIT, T.I);
  }

  /// @}
  /// \name Mathematics Operations
  /// @{

  /// \returns the floor log base 2 of this GenericAPInt.
  unsigned logBase2() const { return BitWidth - 1 - countLeadingZeros(); }

  /// \returns the ceil log base 2 of this GenericAPInt.
  unsigned ceilLogBase2() const {
    GenericAPInt temp(*this);
    --temp;
    return BitWidth - temp.countLeadingZeros();
  }

  /// \returns the nearest log base 2 of this GenericAPInt. Ties round up.
  ///
  /// NOTE: When we have a BitWidth of 1, we define:
  ///
  ///   log2(0) = UINT32_MAX
  ///   log2(1) = 0
  ///
  /// to get around any mathematical concerns resulting from
  /// referencing 2 in a space where 2 does no exist.
  unsigned nearestLogBase2() const {
    // Special case when we have a bitwidth of 1. If VAL is 1, then we
    // get 0. If VAL is 0, we get UINT64_MAX which gets truncated to
    // UINT32_MAX.
    if (BitWidth == 1)
      return VAL[0] - 1;

    // Handle the zero case.
    if (!getBoolValue())
      return UINT32_MAX;

    // The non-zero case is handled by computing:
    //
    //   nearestLogBase2(x) = logBase2(x) + x[logBase2(x)-1].
    //
    // where x[i] is referring to the value of the ith bit of x.
    unsigned lg = logBase2();
    return lg + unsigned((*this)[lg - 1]);
  }

  /// \returns the log base 2 of this GenericAPInt if its an exact power of two, -1
  /// otherwise
  int32_t exactLogBase2() const {
    if (!isPowerOf2())
      return -1;
    return logBase2();
  }

  // Square Root - this method computes and returns the square root of "this".
  // Three mechanisms are used for computation. For small values (<= 5 bits),
  // a table lookup is done. This gets some performance for common cases. For
  // values using less than 52 bits, the value is converted to double and then
  // the libc sqrt function is called. The result is rounded and then converted
  // back to a uint64_t which is then used to construct the result. Finally,
  // the Babylonian method for computing square roots is used.
  /// \brief Compute the square root
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT sqrt() const {
    // Determine the magnitude of the value.
    unsigned magnitude = getActiveBits();

    // Use a fast table for some small values. This also gets rid of some
    // rounding errors in libc sqrt for small values.
    if (magnitude <= 5) {
      static const uint8_t results[32] = {
          /*     0 */ 0,
          /*  1- 2 */ 1, 1,
          /*  3- 6 */ 2, 2, 2, 2,
          /*  7-12 */ 3, 3, 3, 3, 3, 3,
          /* 13-20 */ 4, 4, 4, 4, 4, 4, 4, 4,
          /* 21-30 */ 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
          /*    31 */ 6};
      return GenericAPInt(BitWidth, results[words()[0]]);
    }

    // If the magnitude of the value fits in less than 52 bits (the precision of
    // an IEEE double precision floating point value), then we can use the
    // libc sqrt function which will probably use a hardware sqrt computation.
    // This should be faster than the algorithm below.
    if (magnitude < 52) {
      return GenericAPInt(BitWidth,
                          uint64_t(::round(::sqrt(double(words()[0])))));
    }

    // Okay, all the short cuts are exhausted. We must compute it. The following
    // is a classical Babylonian method for computing the square root. This code
    // was adapted to GenericAPInt from a wikipedia article on such
    // computations.
    // See http://www.wikipedia.org/ and go to the page named
    // Calculate_an_integer_square_root.
    unsigned nbits = BitWidth, i = 4;
    GenericAPInt testy(BitWidth, 16);
    GenericAPInt x_old(BitWidth, 1);
    GenericAPInt x_new(BitWidth, 0);
    GenericAPInt two(BitWidth, 2);

    // Select a good starting value using binary logarithms.
    for (;; i += 2, testy = testy.shl(2))
      if (i >= nbits || this->ule(testy)) {
        x_old = x_old.shl(i / 2);
        break;
      }

    // Use the Babylonian method to arrive at the integer square root:
    for (;;) {
      x_new = (this->udiv(x_old) + x_old).udiv(two);
      if (x_old.ule(x_new))
        break;
      x_old = x_new;
    }

    // Make sure we return the closest approximation
    // NOTE: The rounding calculation below is correct. It will produce an
    // off-by-one discrepancy with results from pari/gp. That discrepancy has
    // been
    // determined to be a rounding issue with pari/gp as it begins to use a
    // floating point representation after 192 bits. There are no discrepancies
    // between this algorithm and pari/gp for bit widths < 192 bits.
    GenericAPInt square(x_old * x_old);
    GenericAPInt nextSquare((x_old + 1) * (x_old + 1));
    if (this->ult(square))
      return x_old;
    assert(this->ule(nextSquare) && "Error in GenericAPInt::sqrt computation");
    GenericAPInt midpoint((nextSquare - square).udiv(two));
    GenericAPInt offset(*this - square);
    if (offset.ult(midpoint))
      return x_old;
    return x_old + 1;
  }

  /// \brief Get the absolute value;
  ///
  /// If *this is < 0 then return -(*this), otherwise *this;
  GenericAPInt LLVM_ATTRIBUTE_UNUSED_RESULT abs() const {
    if (isNegative())
      return -(*this);
    return *this;
  }

  /// Computes the multiplicative inverse of this APInt for a given modulo. The
  /// iterative extended Euclidean algorithm is used to solve for this value,
  /// however we simplify it to speed up calculating only the inverse, and take
  /// advantage of div+rem calculations. We also use some tricks to avoid
  /// copying (potentially large) APInts around.
  /// \returns the multiplicative inverse for a given modulo.
  GenericAPInt multiplicativeInverse(const GenericAPInt &modulo) const {
    assert(ult(modulo) && "This GenericAPInt must be smaller than the modulo");

    // Using the properties listed at the following web page (accessed
    // 06/21/08):
    //   http://www.numbertheory.org/php/euclid.html
    // (especially the properties numbered 3, 4 and 9) it can be proved that
    // BitWidth bits suffice for all the computations in the algorithm
    // implemented
    // below. More precisely, this number of bits suffice if the multiplicative
    // inverse exists, but may not suffice for the general extended Euclidean
    // algorithm.

    GenericAPInt r[2] = {modulo, *this};
    GenericAPInt t[2] = {GenericAPInt(BitWidth, 0), GenericAPInt(BitWidth, 1)};
    GenericAPInt q(BitWidth, 0);

    unsigned i;
    for (i = 0; r[i ^ 1] != 0; i ^= 1) {
      // An overview of the math without the confusing bit-flipping:
      // q = r[i-2] / r[i-1]
      // r[i] = r[i-2] % r[i-1]
      // t[i] = t[i-2] - t[i-1] * q
      udivrem(r[i], r[i ^ 1], q, r[i]);
      t[i] -= t[i ^ 1] * q;
    }

    // If this GenericAPInt and the modulo are not coprime, there is no
    // multiplicative inverse, so return 0. We check this by looking at the
    // next-to-last remainder, which is the gcd(*this,modulo) as calculated by
    // the Euclidean algorithm.
    if (r[i] != 1)
      return GenericAPInt(BitWidth, 0);

    // The next-to-last t is the multiplicative inverse.  However, we are
    // interested in a positive inverse. Calcuate a positive one from a negative
    // one if necessary. A simple addition of the modulo suffices because
    // abs(t[i]) is known to be less than *this/2 (see the link above).
    return t[i].isNegative() ? t[i] + modulo : t[i];
  }

  /// @}
  /// \name Support for division by constant
  /// @{

  /// Magic data for optimising signed division by a constant.
  struct ms {
    GenericAPInt m; ///< magic number
    unsigned s;     ///< shift amount
  };
  /// Calculate the magic numbers required to implement a signed integer
  /// division by a constant as a sequence of multiplies, adds and shifts.
  /// Requires that the divisor not be 0, 1, or -1.  Taken from "Hacker's
  /// Delight", Henry S. Warren, Jr., chapter 10.
  ms magic() const {
    const GenericAPInt &d = *this;
    unsigned p;
    GenericAPInt ad, anc, delta, q1, r1, q2, r2, t;
    GenericAPInt signedMin = GenericAPInt::getSignedMinValue(d.getBitWidth());
    struct ms mag;

    ad = d.abs();
    t = signedMin + (d.lshr(d.getBitWidth() - 1));
    anc = t - 1 - t.urem(ad);  // absolute value of nc
    p = d.getBitWidth() - 1;   // initialize p
    q1 = signedMin.udiv(anc);  // initialize q1 = 2p/abs(nc)
    r1 = signedMin - q1 * anc; // initialize r1 = rem(2p,abs(nc))
    q2 = signedMin.udiv(ad);   // initialize q2 = 2p/abs(d)
    r2 = signedMin - q2 * ad;  // initialize r2 = rem(2p,abs(d))
    do {
      p = p + 1;
      q1 = q1 << 1;      // update q1 = 2p/abs(nc)
      r1 = r1 << 1;      // update r1 = rem(2p/abs(nc))
      if (r1.uge(anc)) { // must be unsigned comparison
        q1 = q1 + 1;
        r1 = r1 - anc;
      }
      q2 = q2 << 1;     // update q2 = 2p/abs(d)
      r2 = r2 << 1;     // update r2 = rem(2p/abs(d))
      if (r2.uge(ad)) { // must be unsigned comparison
        q2 = q2 + 1;
        r2 = r2 - ad;
      }
      delta = ad - r2;
    } while (q1.ult(delta) || (q1 == delta && r1 == 0));

    mag.m = q2 + 1;
    if (d.isNegative())
      mag.m = -mag.m;            // resulting magic number
    mag.s = p - d.getBitWidth(); // resulting shift
    return mag;
  }

  /// Magic data for optimising unsigned division by a constant.
  struct mu {
    GenericAPInt m; ///< magic number
    bool a;         ///< add indicator
    unsigned s;     ///< shift amount
  };
  /// Calculate the magic numbers required to implement an unsigned integer
  /// division by a constant as a sequence of multiplies, adds and shifts.
  /// Requires that the divisor not be 0.  Taken from "Hacker's Delight", Henry
  /// S. Warren, Jr., chapter 10.
  /// LeadingZeros can be used to simplify the calculation if the upper bits
  /// of the divided value are known zero.
  mu magicu(unsigned LeadingZeros = 0) const {
    const GenericAPInt &d = *this;
    unsigned p;
    GenericAPInt nc, delta, q1, r1, q2, r2;
    struct mu magu;
    magu.a = 0; // initialize "add" indicator
    GenericAPInt allOnes =
        GenericAPInt::getAllOnesValue(d.getBitWidth()).lshr(LeadingZeros);
    GenericAPInt signedMin = GenericAPInt::getSignedMinValue(d.getBitWidth());
    GenericAPInt signedMax = GenericAPInt::getSignedMaxValue(d.getBitWidth());

    nc = allOnes - (allOnes - d).urem(d);
    p = d.getBitWidth() - 1;  // initialize p
    q1 = signedMin.udiv(nc);  // initialize q1 = 2p/nc
    r1 = signedMin - q1 * nc; // initialize r1 = rem(2p,nc)
    q2 = signedMax.udiv(d);   // initialize q2 = (2p-1)/d
    r2 = signedMax - q2 * d;  // initialize r2 = rem((2p-1),d)
    do {
      p = p + 1;
      if (r1.uge(nc - r1)) {
        q1 = q1 + q1 + 1;  // update q1
        r1 = r1 + r1 - nc; // update r1
      } else {
        q1 = q1 + q1; // update q1
        r1 = r1 + r1; // update r1
      }
      if ((r2 + 1).uge(d - r2)) {
        if (q2.uge(signedMax))
          magu.a = 1;
        q2 = q2 + q2 + 1;     // update q2
        r2 = r2 + r2 + 1 - d; // update r2
      } else {
        if (q2.uge(signedMin))
          magu.a = 1;
        q2 = q2 + q2;     // update q2
        r2 = r2 + r2 + 1; // update r2
      }
      delta = d - 1 - r2;
    } while (p < d.getBitWidth() * 2 &&
             (q1.ult(delta) || (q1 == delta && r1 == 0)));
    magu.m = q2 + 1;              // resulting magic number
    magu.s = p - d.getBitWidth(); // resulting shift
    return magu;
  }

  /// \brief debug method
  void dump() const { APIntRef(*this).dump(); }

  /// @}
};

namespace tc {
/// \name Building-block Operations for GenericAPInt and APFloat
/// @{

// These building block operations operate on a representation of arbitrary
// precision, two's-complement, bignum integer values. They should be
// sufficient to implement GenericAPInt and APFloat bignum requirements. Inputs
// are
// generally a pointer to the base of an array of integer parts, representing
// an unsigned bignum, and a count of how many parts there are.

/// Sets the least significant part of a bignum to the input value, and zeroes
/// out higher parts.
void Set(integerPart *, integerPart, unsigned int);

/// Assign one bignum to another.
void Assign(integerPart *, const integerPart *, unsigned int);

/// Returns true if a bignum is zero, false otherwise.
bool IsZero(const integerPart *, unsigned int);

/// Extract the given bit of a bignum; returns 0 or 1.  Zero-based.
int ExtractBit(const integerPart *, unsigned int bit);

/// Copy the bit vector of width srcBITS from SRC, starting at bit srcLSB, to
/// DST, of dstCOUNT parts, such that the bit srcLSB becomes the least
/// significant bit of DST.  All high bits above srcBITS in DST are
/// zero-filled.
void Extract(integerPart *, unsigned int dstCount, const integerPart *,
             unsigned int srcBits, unsigned int srcLSB);

/// Set the given bit of a bignum.  Zero-based.
void SetBit(integerPart *, unsigned int bit);

/// Clear the given bit of a bignum.  Zero-based.
void ClearBit(integerPart *, unsigned int bit);

/// Returns the bit number of the least or most significant set bit of a
/// number.  If the input number has no bits set -1U is returned.
unsigned int LSB(const integerPart *, unsigned int);
unsigned int MSB(const integerPart *parts, unsigned int n);

/// Negate a bignum in-place.
void Negate(integerPart *, unsigned int);

/// DST += RHS + CARRY where CARRY is zero or one.  Returns the carry flag.
integerPart Add(integerPart *, const integerPart *, integerPart carry,
                unsigned);

/// DST -= RHS + CARRY where CARRY is zero or one. Returns the carry flag.
integerPart Subtract(integerPart *, const integerPart *, integerPart carry,
                     unsigned);

/// DST += SRC * MULTIPLIER + PART   if add is true
/// DST  = SRC * MULTIPLIER + PART   if add is false
///
/// Requires 0 <= DSTPARTS <= SRCPARTS + 1.  If DST overlaps SRC they must
/// start at the same point, i.e. DST == SRC.
///
/// If DSTPARTS == SRC_PARTS + 1 no overflow occurs and zero is returned.
/// Otherwise DST is filled with the least significant DSTPARTS parts of the
/// result, and if all of the omitted higher parts were zero return zero,
/// otherwise overflow occurred and return one.
int MultiplyPart(integerPart *dst, const integerPart *src,
                 integerPart multiplier, integerPart carry,
                 unsigned int srcParts, unsigned int dstParts, bool add);

/// DST = LHS * RHS, where DST has the same width as the operands and is
/// filled with the least significant parts of the result.  Returns one if
/// overflow occurred, otherwise zero.  DST must be disjoint from both
/// operands.
int Multiply(integerPart *, const integerPart *, const integerPart *, unsigned);

/// DST = LHS * RHS, where DST has width the sum of the widths of the
/// operands.  No overflow occurs.  DST must be disjoint from both
/// operands. Returns the number of parts required to hold the result.
unsigned int FullMultiply(integerPart *, const integerPart *,
                          const integerPart *, unsigned, unsigned);

/// If RHS is zero LHS and REMAINDER are left unchanged, return one.
/// Otherwise set LHS to LHS / RHS with the fractional part discarded, set
/// REMAINDER to the remainder, return zero.  i.e.
///
///  OLD_LHS = RHS * LHS + REMAINDER
///
/// SCRATCH is a bignum of the same size as the operands and result for use by
/// the routine; its contents need not be initialized and are destroyed.  LHS,
/// REMAINDER and SCRATCH must be distinct.
int Divide(integerPart *lhs, const integerPart *rhs, integerPart *remainder,
           integerPart *scratch, unsigned int parts);

/// Shift a bignum left COUNT bits.  Shifted in bits are zero.  There are no
/// restrictions on COUNT.
void ShiftLeft(integerPart *, unsigned int parts, unsigned int count);

/// Shift a bignum right COUNT bits.  Shifted in bits are zero.  There are no
/// restrictions on COUNT.
void ShiftRight(integerPart *, unsigned int parts, unsigned int count);

/// The obvious AND, OR and XOR and complement operations.
void And(integerPart *, const integerPart *, unsigned int);
void Or(integerPart *, const integerPart *, unsigned int);
void Xor(integerPart *, const integerPart *, unsigned int);
void Complement(integerPart *, unsigned int);

/// Comparison (unsigned) of two bignums.
int Compare(const integerPart *, const integerPart *, unsigned int);

/// Increment a bignum in-place.  Return the carry flag.
integerPart Increment(integerPart *, unsigned int);

/// Decrement a bignum in-place.  Return the borrow flag.
integerPart Decrement(integerPart *, unsigned int);

/// Set the least significant BITS and clear the rest.
void SetLeastSignificantBits(integerPart *, unsigned int, unsigned int bits);

/// @}
};

typedef GenericAPInt<1> APInt;

template <unsigned NumWords>
inline bool operator==(uint64_t V1, const GenericAPInt<NumWords> &V2) {
  return V2 == V1;
}

template <unsigned NumWords>
inline bool operator!=(uint64_t V1, const GenericAPInt<NumWords> &V2) {
  return V2 != V1;
}

inline raw_ostream &operator<<(raw_ostream &OS, APInt I) {
  I.print(OS, true);
  return OS;
}

template <unsigned NumWords>
inline GenericAPInt<NumWords> operator-(GenericAPInt<NumWords> v) {
  v.flipAllBits();
  ++v;
  return v;
}

template <unsigned NumWords>
inline GenericAPInt<NumWords> operator+(GenericAPInt<NumWords> a,
                                        const GenericAPInt<NumWords> &b) {
  a += b;
  return a;
}

template <unsigned NumWords>
inline GenericAPInt<NumWords> operator+(const GenericAPInt<NumWords> &a,
                                        GenericAPInt<NumWords> &&b) {
  b += a;
  return std::move(b);
}

template <unsigned NumWords>
inline GenericAPInt<NumWords> operator+(GenericAPInt<NumWords> a,
                                        uint64_t RHS) {
  a += RHS;
  return a;
}

template <unsigned NumWords>
inline GenericAPInt<NumWords> operator+(uint64_t LHS,
                                        GenericAPInt<NumWords> b) {
  b += LHS;
  return b;
}

template <unsigned NumWords>
inline GenericAPInt<NumWords> operator-(GenericAPInt<NumWords> a,
                                        const GenericAPInt<NumWords> &b) {
  a -= b;
  return a;
}

template <unsigned NumWords>
inline GenericAPInt<NumWords> operator-(const GenericAPInt<NumWords> &a,
                                        GenericAPInt<NumWords> &&b) {
  b = -std::move(b);
  b += a;
  return std::move(b);
}

template <unsigned NumWords>
inline GenericAPInt<NumWords> operator-(GenericAPInt<NumWords> a,
                                        uint64_t RHS) {
  a -= RHS;
  return a;
}

template <unsigned NumWords>
inline GenericAPInt<NumWords> operator-(uint64_t LHS,
                                        GenericAPInt<NumWords> b) {
  b = -std::move(b);
  b += LHS;
  return b;
}

namespace APIntOps {

/// \brief Determine the smaller of two APInts considered to be signed.
template <unsigned NumWords>
inline const GenericAPInt<NumWords> &smin(const GenericAPInt<NumWords> &A,
                                          const GenericAPInt<NumWords> &B) {
  return A.slt(B) ? A : B;
}

/// \brief Determine the larger of two APInts considered to be signed.
template <unsigned NumWords>
inline const GenericAPInt<NumWords> &smax(const GenericAPInt<NumWords> &A,
                                          const GenericAPInt<NumWords> &B) {
  return A.sgt(B) ? A : B;
}

/// \brief Determine the smaller of two APInts considered to be signed.
template <unsigned NumWords>
inline const GenericAPInt<NumWords> &umin(const GenericAPInt<NumWords> &A,
                                          const GenericAPInt<NumWords> &B) {
  return A.ult(B) ? A : B;
}

/// \brief Determine the larger of two APInts considered to be unsigned.
template <unsigned NumWords>
inline const GenericAPInt<NumWords> &umax(const GenericAPInt<NumWords> &A,
                                          const GenericAPInt<NumWords> &B) {
  return A.ugt(B) ? A : B;
}

/// \brief Check if the specified GenericAPInt<NumWords> has a N-bits unsigned
/// integer value.
template <unsigned NumWords>
inline bool isIntN(unsigned N, const GenericAPInt<NumWords> &APIVal) {
  return APIVal.isIntN(N);
}

/// \brief Check if the specified GenericAPInt<NumWords> has a N-bits signed
/// integer value.
template <unsigned NumWords>
inline bool isSignedIntN(unsigned N, const GenericAPInt<NumWords> &APIVal) {
  return APIVal.isSignedIntN(N);
}

/// \returns true if the argument GenericAPInt<NumWords> value is a sequence of
/// ones starting at
/// the least significant bit with the remainder zero.
template <unsigned NumWords>
inline bool isMask(unsigned numBits, const GenericAPInt<NumWords> &APIVal) {
  return numBits <= APIVal.getBitWidth() &&
         APIVal == GenericAPInt<NumWords>::getLowBitsSet(APIVal.getBitWidth(),
                                                         numBits);
}

/// \returns true if the argument is a non-empty sequence of ones starting at
/// the least significant bit with the remainder zero (32 bit version).
/// Ex. isMask(0x0000FFFFU) == true.
template <unsigned NumWords>
inline bool isMask(const GenericAPInt<NumWords> &Value) {
  return (Value != 0) && ((Value + 1) & Value) == 0;
}

/// \brief Return true if the argument GenericAPInt value contains a sequence of
/// ones with the remainder zero.
template <unsigned NumWords>
inline bool isShiftedMask(unsigned numBits,
                          const GenericAPInt<NumWords> &APIVal) {
  return isMask(numBits,
                (APIVal - GenericAPInt<NumWords>(numBits, 1)) | APIVal);
}

/// \brief Returns a byte-swapped representation of the specified GenericAPInt
/// Value.
template <unsigned NumWords>
inline GenericAPInt<NumWords> byteSwap(const GenericAPInt<NumWords> &APIVal) {
  return APIVal.byteSwap();
}

/// \brief Returns the floor log base 2 of the specified GenericAPInt value.
template <unsigned NumWords>
inline unsigned logBase2(const GenericAPInt<NumWords> &APIVal) {
  return APIVal.logBase2();
}

/// \brief Converts the given GenericAPInt to a double value.
///
/// Treats the GenericAPInt as an unsigned value for conversion
/// purposes.
template <unsigned NumWords>
inline double RoundAPIntToDouble(const GenericAPInt<NumWords> &APIVal) {
  return APIVal.roundToDouble();
}

/// \brief Converts the given GenericAPInt to a double value.
///
/// Treats the GenericAPInt as a signed value for conversion purposes.
template <unsigned NumWords>
inline double RoundSignedAPIntToDouble(const GenericAPInt<NumWords> &APIVal) {
  return APIVal.signedRoundToDouble();
}

/// \brief Converts the given GenericAPInt<NumWords> to a float vlalue.
template <unsigned NumWords>
inline float RoundAPIntToFloat(const GenericAPInt<NumWords> &APIVal) {
  return float(RoundAPIntToDouble(APIVal));
}

/// \brief Converts the given GenericAPInt to a float value.
///
/// Treats the GenericAPInt as a signed value for conversion purposes.
template <unsigned NumWords>
inline float RoundSignedAPIntToFloat(const GenericAPInt<NumWords> &APIVal) {
  return float(APIVal.signedRoundToDouble());
}

/// \brief Converts the given double value into an APInt.
///
/// This function convert a double value to an APInt value.
APInt RoundDoubleToAPInt(double Double, unsigned width);

/// \brief Converts a float value into an APInt.
///
/// Converts a float value into an APInt value.
inline APInt RoundFloatToAPInt(float Float, unsigned width) {
  return RoundDoubleToAPInt(double(Float), width);
}

/// \brief Arithmetic right-shift function.
///
/// Arithmetic right-shift the GenericAPInt by shiftAmt.
template <unsigned NumWords>
inline GenericAPInt<NumWords> ashr(const GenericAPInt<NumWords> &LHS,
                                   unsigned shiftAmt) {
  return LHS.ashr(shiftAmt);
}

/// \brief Logical right-shift function.
///
/// Logical right-shift the GenericAPInt by shiftAmt.
template <unsigned NumWords>
inline GenericAPInt<NumWords> lshr(const GenericAPInt<NumWords> &LHS,
                                   unsigned shiftAmt) {
  return LHS.lshr(shiftAmt);
}

/// \brief Left-shift function.
///
/// Left-shift the GenericAPInt by shiftAmt.
template <unsigned NumWords>
inline GenericAPInt<NumWords> shl(const GenericAPInt<NumWords> &LHS,
                                  unsigned shiftAmt) {
  return LHS.shl(shiftAmt);
}

/// \brief Signed division function for GenericAPInt<NumWords>.
///
/// Signed divide GenericAPInt<NumWords> LHS by GenericAPInt<NumWords> RHS.
template <unsigned NumWords>
inline GenericAPInt<NumWords> sdiv(const GenericAPInt<NumWords> &LHS,
                                   const GenericAPInt<NumWords> &RHS) {
  return LHS.sdiv(RHS);
}

/// \brief Unsigned division function for GenericAPInt<NumWords>.
///
/// Unsigned divide GenericAPInt<NumWords> LHS by GenericAPInt<NumWords> RHS.
template <unsigned NumWords>
inline GenericAPInt<NumWords> udiv(const GenericAPInt<NumWords> &LHS,
                                   const GenericAPInt<NumWords> &RHS) {
  return LHS.udiv(RHS);
}

/// \brief Function for signed remainder operation.
///
/// Signed remainder operation on GenericAPInt<NumWords>.
template <unsigned NumWords>
inline GenericAPInt<NumWords> srem(const GenericAPInt<NumWords> &LHS,
                                   const GenericAPInt<NumWords> &RHS) {
  return LHS.srem(RHS);
}

/// \brief Function for unsigned remainder operation.
///
/// Unsigned remainder operation on GenericAPInt<NumWords>.
template <unsigned NumWords>
inline GenericAPInt<NumWords> urem(const GenericAPInt<NumWords> &LHS,
                                   const GenericAPInt<NumWords> &RHS) {
  return LHS.urem(RHS);
}

/// \brief Compute GCD of two GenericAPInt<NumWords> values.
///
/// This function returns the greatest common divisor of the two
/// GenericAPInt<NumWords> values
/// using Euclid's algorithm.
///
/// \returns the greatest common divisor of Val1 and Val2
template <unsigned NumWords>
GenericAPInt<NumWords>
GreatestCommonDivisor(const GenericAPInt<NumWords> &Val1,
                      const GenericAPInt<NumWords> &Val2) {
  GenericAPInt<NumWords> A = Val1, B = Val2;
  while (!!B) {
    GenericAPInt<NumWords> T = B;
    B = APIntOps::urem(A, B);
    A = T;
  }
  return A;
}

/// \brief Function for multiplication operation.
///
/// Performs multiplication on GenericAPInt values.
template <unsigned NumWords>
inline GenericAPInt<NumWords> mul(const GenericAPInt<NumWords> &LHS,
                                  const GenericAPInt<NumWords> &RHS) {
  return LHS * RHS;
}

/// \brief Function for addition operation.
///
/// Performs addition on GenericAPInt<NumWords> values.
template <unsigned NumWords>
inline GenericAPInt<NumWords> add(const GenericAPInt<NumWords> &LHS,
                                  const GenericAPInt<NumWords> &RHS) {
  return LHS + RHS;
}

/// \brief Function for subtraction operation.
///
/// Performs subtraction on GenericAPInt<NumWords> values.
template <unsigned NumWords>
inline GenericAPInt<NumWords> sub(const GenericAPInt<NumWords> &LHS,
                                  const GenericAPInt<NumWords> &RHS) {
  return LHS - RHS;
}

/// \brief Bitwise AND function for GenericAPInt<NumWords>.
///
/// Performs bitwise AND operation on GenericAPInt<NumWords> LHS and
/// GenericAPInt<NumWords> RHS.
template <unsigned NumWords>
inline GenericAPInt<NumWords> And(const GenericAPInt<NumWords> &LHS,
                                  const GenericAPInt<NumWords> &RHS) {
  return LHS & RHS;
}

/// \brief Bitwise OR function for GenericAPInt<NumWords>.
///
/// Performs bitwise OR operation on GenericAPInt<NumWords> LHS and
/// GenericAPInt<NumWords> RHS.
template <unsigned NumWords>
inline GenericAPInt<NumWords> Or(const GenericAPInt<NumWords> &LHS,
                                 const GenericAPInt<NumWords> &RHS) {
  return LHS | RHS;
}

/// \brief Bitwise XOR function for GenericAPInt<NumWords>.
///
/// Performs bitwise XOR operation on GenericAPInt<NumWords>.
template <unsigned NumWords>
inline GenericAPInt<NumWords> Xor(const GenericAPInt<NumWords> &LHS,
                                  const GenericAPInt<NumWords> &RHS) {
  return LHS ^ RHS;
}

/// \brief Bitwise complement function.
///
/// Performs a bitwise complement operation on GenericAPInt<NumWords>.
template <unsigned NumWords>
inline GenericAPInt<NumWords> Not(const GenericAPInt<NumWords> &APIVal) {
  return ~APIVal;
}

} // End of APIntOps namespace

// See friend declaration above. This additional declaration is required in
// order to compile LLVM with IBM xlC compiler.
hash_code hash_value(const APInt &Arg);
} // End of llvm namespace

#endif
