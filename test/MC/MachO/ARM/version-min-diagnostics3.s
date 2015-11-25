// RUN: llvm-mc -triple armv7s-apple-ios %s 2>&1 | FileCheck --check-prefix=CHECK --check-prefix=VNONE %s
// RUN: llvm-mc -triple armv7s-apple-ios9.0.1 %s 2>&1 | FileCheck --check-prefix=CHECK --check-prefix=VNINE --check-prefix=VNINE_MICRO %s
// RUN: llvm-mc -triple armv7s-apple-ios9.0 %s 2>&1 | FileCheck --check-prefix=CHECK --check-prefix=VNINE --check-prefix=VNINE_NOMICRO %s

// VNONE-NOT: version-min-diagnostics3.s:[[@LINE+2]]{{.*}}does not match target version
// VNINE: version-min-diagnostics3.s:[[@LINE+1]]:1: warning: .ios_version_min 8,0,0 does not match target version 9,
.ios_version_min 8,0
// VNONE-NOT: version-min-diagnostics3.s:[[@LINE+2]]{{.*}}does not match target version
// VNINE: version-min-diagnostics3.s:[[@LINE+1]]:1: warning: .ios_version_min 10,0,0 does not match target version 9,
.ios_version_min 10,0
// VNONE-NOT: version-min-diagnostics3.s:[[@LINE+2]]{{.*}}does not match target version
// VNINE: version-min-diagnostics3.s:[[@LINE+1]]:1: warning: .ios_version_min 9,1,0 does not match target version 9,
.ios_version_min 9,1
// VNONE-NOT: version-min-diagnostics3.s:[[@LINE+3]]{{.*}}does not match target version
// VNINE_NOMICRO-NOT: version-min-diagnostics3.s:[[@LINE+2]]{{.*}}does not match target version
// VNINE_MICRO: version-min-diagnostics3.s:[[@LINE+1]]:1: warning: .ios_version_min 9,0,2 does not match target version 9,0,1
.ios_version_min 9,0,2

// CHECK-NOT: version-min-diagnostics3.s:[[@LINE+1]]{{.*}}does not match target version
.ios_version_min 9,0
// CHECK-NOT: version-min-diagnostics3.s:[[@LINE+1]]{{.*}}does not match target version
.ios_version_min 9,0,1
