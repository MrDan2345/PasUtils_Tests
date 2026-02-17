program MatInverseTest;

{$include PasUtilsMode.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CommonUtils,
  CryptoUtils, Backup;

function UMatInverse_Test(const m: TUMat): TUMat; assembler;
const One: Single = 1;
var
  Vec0, Vec1, Vec2, Vec3: TUVec4;
  Inv0, Inv1, Inv2, Inv3: TUVec4;
  Row0: TUVec4;
  Coef00, Coef02, Coef03: Single;
  Coef04, Coef06, Coef07: Single;
  Coef08, Coef10, Coef11: Single;
  Coef12, Coef14, Coef15: Single;
  Coef16, Coef18, Coef19: Single;
  Coef20, Coef22, Coef23: Single;
  Dot0: Single;
  InvDet: Single;
asm
  mov rax, m  // rax = source matrix
  mov rdx, Result  // rdx = result matrix

  // ============================================================================
  // Calculate all Coef values
  // ============================================================================

  // Coef00 = m[2,2] * m[3,3] - m[3,2] * m[2,3]
  movss xmm0, [rax+40]    // m[2,2]
  movss xmm1, [rax+60]    // m[3,3]
  mulss xmm0, xmm1
  movss xmm1, [rax+56]    // m[3,2]
  movss xmm2, [rax+44]    // m[2,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss dword ptr [Coef00], xmm0       // Coef00

  // Coef02 = m[1,2] * m[3,3] - m[3,2] * m[1,3]
  movss xmm0, [rax+24]    // m[1,2]
  movss xmm1, [rax+60]    // m[3,3]
  mulss xmm0, xmm1
  movss xmm1, [rax+56]    // m[3,2]
  movss xmm2, [rax+28]    // m[1,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef02], xmm0     // Coef02

  // Coef03 = m[1,2] * m[2,3] - m[2,2] * m[1,3]
  movss xmm0, [rax+24]    // m[1,2]
  movss xmm1, [rax+44]    // m[2,3]
  mulss xmm0, xmm1
  movss xmm1, [rax+40]    // m[2,2]
  movss xmm2, [rax+28]    // m[1,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef03], xmm0     // Coef03

  // Coef04 = m[2,1] * m[3,3] - m[3,1] * m[2,3]
  movss xmm0, [rax+36]    // m[2,1]
  movss xmm1, [rax+60]    // m[3,3]
  mulss xmm0, xmm1
  movss xmm1, [rax+52]    // m[3,1]
  movss xmm2, [rax+44]    // m[2,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef04], xmm0    // Coef04

  // Coef06 = m[1,1] * m[3,3] - m[3,1] * m[1,3]
  movss xmm0, [rax+20]    // m[1,1]
  movss xmm1, [rax+60]    // m[3,3]
  mulss xmm0, xmm1
  movss xmm1, [rax+52]    // m[3,1]
  movss xmm2, [rax+28]    // m[1,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef06], xmm0    // Coef06

  // Coef07 = m[1,1] * m[2,3] - m[2,1] * m[1,3]
  movss xmm0, [rax+20]    // m[1,1]
  movss xmm1, [rax+44]    // m[2,3]
  mulss xmm0, xmm1
  movss xmm1, [rax+36]    // m[2,1]
  movss xmm2, [rax+28]    // m[1,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef07], xmm0    // Coef07

  // Coef08 = m[2,1] * m[3,2] - m[3,1] * m[2,2]
  movss xmm0, [rax+36]    // m[2,1]
  movss xmm1, [rax+56]    // m[3,2]
  mulss xmm0, xmm1
  movss xmm1, [rax+52]    // m[3,1]
  movss xmm2, [rax+40]    // m[2,2]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef08], xmm0    // Coef08

  // Coef10 = m[1,1] * m[3,2] - m[3,1] * m[1,2]
  movss xmm0, [rax+20]    // m[1,1]
  movss xmm1, [rax+56]    // m[3,2]
  mulss xmm0, xmm1
  movss xmm1, [rax+52]    // m[3,1]
  movss xmm2, [rax+24]    // m[1,2]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef10], xmm0    // Coef10

  // Coef11 = m[1,1] * m[2,2] - m[2,1] * m[1,2]
  movss xmm0, [rax+20]    // m[1,1]
  movss xmm1, [rax+40]    // m[2,2]
  mulss xmm0, xmm1
  movss xmm1, [rax+36]    // m[2,1]
  movss xmm2, [rax+24]    // m[1,2]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef11], xmm0    // Coef11

  // Coef12 = m[2,0] * m[3,3] - m[3,0] * m[2,3]
  movss xmm0, [rax+32]    // m[2,0]
  movss xmm1, [rax+60]    // m[3,3]
  mulss xmm0, xmm1
  movss xmm1, [rax+48]    // m[3,0]
  movss xmm2, [rax+44]    // m[2,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef12], xmm0    // Coef12

  // Coef14 = m[1,0] * m[3,3] - m[3,0] * m[1,3]
  movss xmm0, [rax+16]    // m[1,0]
  movss xmm1, [rax+60]    // m[3,3]
  mulss xmm0, xmm1
  movss xmm1, [rax+48]    // m[3,0]
  movss xmm2, [rax+28]    // m[1,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef14], xmm0    // Coef14

  // Coef15 = m[1,0] * m[2,3] - m[2,0] * m[1,3]
  movss xmm0, [rax+16]    // m[1,0]
  movss xmm1, [rax+44]    // m[2,3]
  mulss xmm0, xmm1
  movss xmm1, [rax+32]    // m[2,0]
  movss xmm2, [rax+28]    // m[1,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef15], xmm0    // Coef15

  // Coef16 = m[2,0] * m[3,2] - m[3,0] * m[2,2]
  movss xmm0, [rax+32]    // m[2,0]
  movss xmm1, [rax+56]    // m[3,2]
  mulss xmm0, xmm1
  movss xmm1, [rax+48]    // m[3,0]
  movss xmm2, [rax+40]    // m[2,2]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef16], xmm0    // Coef16

  // Coef18 = m[1,0] * m[3,2] - m[3,0] * m[1,2]
  movss xmm0, [rax+16]    // m[1,0]
  movss xmm1, [rax+56]    // m[3,2]
  mulss xmm0, xmm1
  movss xmm1, [rax+48]    // m[3,0]
  movss xmm2, [rax+24]    // m[1,2]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef18], xmm0    // Coef18

  // Coef19 = m[1,0] * m[2,2] - m[2,0] * m[1,2]
  movss xmm0, [rax+16]    // m[1,0]
  movss xmm1, [rax+40]    // m[2,2]
  mulss xmm0, xmm1
  movss xmm1, [rax+32]    // m[2,0]
  movss xmm2, [rax+24]    // m[1,2]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef19], xmm0    // Coef19

  // Coef20 = m[2,0] * m[3,1] - m[3,0] * m[2,1]
  movss xmm0, [rax+32]    // m[2,0]
  movss xmm1, [rax+52]    // m[3,1]
  mulss xmm0, xmm1
  movss xmm1, [rax+48]    // m[3,0]
  movss xmm2, [rax+36]    // m[2,1]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef20], xmm0    // Coef20

  // Coef22 = m[1,0] * m[3,1] - m[3,0] * m[1,1]
  movss xmm0, [rax+16]    // m[1,0]
  movss xmm1, [rax+52]    // m[3,1]
  mulss xmm0, xmm1
  movss xmm1, [rax+48]    // m[3,0]
  movss xmm2, [rax+20]    // m[1,1]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef22], xmm0    // Coef22

  // Coef23 = m[1,0] * m[2,1] - m[2,0] * m[1,1]
  movss xmm0, [rax+16]    // m[1,0]
  movss xmm1, [rax+36]    // m[2,1]
  mulss xmm0, xmm1
  movss xmm1, [rax+32]    // m[2,0]
  movss xmm2, [rax+20]    // m[1,1]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef23], xmm0    // Coef23

  // ============================================================================
  // Build Vec0, Vec1, Vec2, Vec3 using SIMD
  // ============================================================================

  // Vec0 = [m[1,0], m[0,0], m[0,0], m[0,0]]
  movss xmm0, [rax+16]    // m[1,0]
  movss xmm1, [rax]       // m[0,0]
  movss xmm2, xmm1
  movss xmm3, xmm1
  unpcklps xmm0, xmm1     // [m[1,0], m[0,0], 0, 0]
  unpcklps xmm2, xmm3     // [m[0,0], m[0,0], 0, 0]
  movlhps xmm0, xmm2      // [m[1,0], m[0,0], m[0,0], m[0,0]]
  movups [Vec0], xmm0   // Vec0

  // Vec1 = [m[1,1], m[0,1], m[0,1], m[0,1]]
  movss xmm0, [rax+20]    // m[1,1]
  movss xmm1, [rax+4]     // m[0,1]
  movss xmm2, xmm1
  movss xmm3, xmm1
  unpcklps xmm0, xmm1
  unpcklps xmm2, xmm3
  movlhps xmm0, xmm2
  movups [Vec1], xmm0   // Vec1

  // Vec2 = [m[1,2], m[0,2], m[0,2], m[0,2]]
  movss xmm0, [rax+24]    // m[1,2]
  movss xmm1, [rax+8]     // m[0,2]
  movss xmm2, xmm1
  movss xmm3, xmm1
  unpcklps xmm0, xmm1
  unpcklps xmm2, xmm3
  movlhps xmm0, xmm2
  movups [Vec2], xmm0  // Vec2

  // Vec3 = [m[1,3], m[0,3], m[0,3], m[0,3]]
  movss xmm0, [rax+28]    // m[1,3]
  movss xmm1, [rax+12]    // m[0,3]
  movss xmm2, xmm1
  movss xmm3, xmm1
  unpcklps xmm0, xmm1
  unpcklps xmm2, xmm3
  movlhps xmm0, xmm2
  movups [Vec3], xmm0  // Vec3

  // ============================================================================
  // Calculate Inv0 using SIMD
  // Inv0[i] = sign[i] * (Vec1[i] * Coef_a - Vec2[i] * Coef_b + Vec3[i] * Coef_c)
  // ============================================================================

  // Inv0[0] = + (Vec1[0] * Coef00 - Vec2[0] * Coef04 + Vec3[0] * Coef08)
  movss xmm0, dword ptr [Vec1 + 0]    // Vec1[0]
  movss xmm1, [Coef00]       // Coef00
  mulss xmm0, xmm1
  movss xmm1, dword ptr [Vec2 + 0]   // Vec2[0]
  movss xmm2, [Coef04]    // Coef04
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, dword ptr [Vec3 + 0]   // Vec3[0]
  movss xmm2, [Coef08]    // Coef08
  mulss xmm1, xmm2
  addss xmm0, xmm1
  movss dword ptr [Inv0 + 0], xmm0   // Inv0[0]

  // Inv0[1] = - (Vec1[1] * Coef00 - Vec2[1] * Coef04 + Vec3[1] * Coef08)
  movss xmm0, dword ptr [Vec1 + 1 * 4]   // Vec1[1]
  movss xmm1, [Coef00]       // Coef00
  mulss xmm0, xmm1
  movss xmm1, dword ptr [Vec2 + 1 * 4]   // Vec2[1]
  movss xmm2, [Coef04]    // Coef04
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, dword ptr [Vec3 + 1 * 4]   // Vec3[1]
  movss xmm2, [Coef08]    // Coef08
  mulss xmm1, xmm2
  addss xmm0, xmm1
  xorps xmm1, xmm1
  subss xmm1, xmm0        // Negate
  movss dword ptr [Inv0 + 1 * 4], xmm1   // Inv0[1]

  // Inv0[2] = + (Vec1[2] * Coef02 - Vec2[2] * Coef06 + Vec3[2] * Coef10)
  movss xmm0, dword ptr [Vec1 + 2 * 4]   // Vec1[2]
  movss xmm1, [Coef02]     // Coef02
  mulss xmm0, xmm1
  movss xmm1, dword ptr [Vec2 + 2 * 4]   // Vec2[2]
  movss xmm2, [Coef06]    // Coef06
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, dword ptr [Vec3 + 2 * 4]   // Vec3[2]
  movss xmm2, [Coef10]    // Coef10
  mulss xmm1, xmm2
  addss xmm0, xmm1
  movss dword ptr [Inv0 + 2 * 4], xmm0   // Inv0[2]

  // Inv0[3] = - (Vec1[3] * Coef03 - Vec2[3] * Coef07 + Vec3[3] * Coef11)
  movss xmm0, dword ptr [Vec1 + 3 * 4]   // Vec1[3]
  movss xmm1, [Coef03]     // Coef03
  mulss xmm0, xmm1
  movss xmm1, dword ptr [Vec2 + 3 * 4]   // Vec2[3]
  movss xmm2, [Coef07]    // Coef07
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, dword ptr [Vec3 + 3 * 4]   // Vec3[3]
  movss xmm2, [Coef11]    // Coef11
  mulss xmm1, xmm2
  addss xmm0, xmm1
  xorps xmm1, xmm1
  subss xmm1, xmm0        // Negate
  movss dword ptr [Inv0 + 3 * 4], xmm1   // Inv0[3]

  // ============================================================================
  // Calculate Inv1
  // ============================================================================

  // Inv1[0] = - (Vec0[0] * Coef00 - Vec2[0] * Coef12 + Vec3[0] * Coef16)
  movss xmm0, dword ptr [Vec0 + 0]    // Vec0[0]
  movss xmm1, [Coef00]       // Coef00
  mulss xmm0, xmm1
  movss xmm1, dword ptr [Vec2 + 0]   // Vec2[0]
  movss xmm2, [Coef12]    // Coef12
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, dword ptr [Vec3 + 0]   // Vec3[0]
  movss xmm2, [Coef16]    // Coef16
  mulss xmm1, xmm2
  addss xmm0, xmm1
  xorps xmm1, xmm1
  subss xmm1, xmm0        // Negate
  movss dword ptr [Inv1 + 0], xmm1   // Inv1[0]

  // Inv1[1] = + (Vec0[1] * Coef00 - Vec2[1] * Coef12 + Vec3[1] * Coef16)
  movss xmm0, dword ptr [Vec0 + 1 * 4]    // Vec0[1]
  movss xmm1, [Coef00]       // Coef00
  mulss xmm0, xmm1
  movss xmm1, dword ptr [Vec2 + 1 * 4]   // Vec2[1]
  movss xmm2, [Coef12]    // Coef12
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, dword ptr [Vec3 + 1 * 4]   // Vec3[1]
  movss xmm2, [Coef16]    // Coef16
  mulss xmm1, xmm2
  addss xmm0, xmm1
  movss dword ptr [Inv1 + 1 * 4], xmm0   // Inv1[1]

  // Inv1[2] = - (Vec0[2] * Coef02 - Vec2[2] * Coef14 + Vec3[2] * Coef18)
  movss xmm0, dword ptr [Vec0 + 2 * 4]    // Vec0[2]
  movss xmm1, [Coef02]     // Coef02
  mulss xmm0, xmm1
  movss xmm1, dword ptr [Vec2 + 2 * 4]   // Vec2[2]
  movss xmm2, [Coef14]    // Coef14
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, dword ptr [Vec3 + 2 * 4]   // Vec3[2]
  movss xmm2, [Coef18]    // Coef18
  mulss xmm1, xmm2
  addss xmm0, xmm1
  xorps xmm1, xmm1
  subss xmm1, xmm0        // Negate
  movss dword ptr [Inv1 + 2 * 4], xmm1   // Inv1[2]

  // Inv1[3] = + (Vec0[3] * Coef03 - Vec2[3] * Coef15 + Vec3[3] * Coef19)
  movss xmm0, dword ptr [Vec0 + 3 * 4]    // Vec0[3]
  movss xmm1, [Coef03]     // Coef03
  mulss xmm0, xmm1
  movss xmm1, dword ptr [Vec2 + 3 * 4]   // Vec2[3]
  movss xmm2, [Coef15]    // Coef15
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, dword ptr [Vec3 + 3 * 4]   // Vec3[3]
  movss xmm2, [Coef19]    // Coef19
  mulss xmm1, xmm2
  addss xmm0, xmm1
  movss dword ptr [Inv1 + 3 * 4], xmm0   // Inv1[3]

  // ============================================================================
  // Calculate Inv2
  // ============================================================================

  // Inv2[0] = + (Vec0[0] * Coef04 - Vec1[0] * Coef12 + Vec3[0] * Coef20)
  movss xmm0, dword ptr [Vec0 + 0]    // Vec0[0]
  movss xmm1, [Coef04]    // Coef04
  mulss xmm0, xmm1
  movss xmm1, dword ptr [Vec1 + 0]    // Vec1[0]
  movss xmm2, [Coef12]    // Coef12
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, dword ptr [Vec3 + 0]   // Vec3[0]
  movss xmm2, [Coef20]    // Coef20
  mulss xmm1, xmm2
  addss xmm0, xmm1
  movss dword ptr [Inv2 + 0], xmm0   // Inv2[0]

  // Inv2[1] = - (Vec0[1] * Coef04 - Vec1[1] * Coef12 + Vec3[1] * Coef20)
  movss xmm0, dword ptr [Vec0 + 1 * 4]    // Vec0[1]
  movss xmm1, [Coef04]    // Coef04
  mulss xmm0, xmm1
  movss xmm1, dword ptr [Vec1 + 1 * 4]   // Vec1[1]
  movss xmm2, [Coef12]    // Coef12
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, dword ptr [Vec3 + 1 * 4]   // Vec3[1]
  movss xmm2, [Coef20]    // Coef20
  mulss xmm1, xmm2
  addss xmm0, xmm1
  xorps xmm1, xmm1
  subss xmm1, xmm0        // Negate
  movss dword ptr [Inv2 + 1 * 4], xmm1   // Inv2[1]

  // Inv2[2] = + (Vec0[2] * Coef06 - Vec1[2] * Coef14 + Vec3[2] * Coef22)
  movss xmm0, dword ptr [Vec0 + 2 * 4]    // Vec0[2]
  movss xmm1, [Coef06]    // Coef06
  mulss xmm0, xmm1
  movss xmm1, dword ptr [Vec1 + 2 * 4]   // Vec1[2]
  movss xmm2, [Coef14]    // Coef14
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, dword ptr [Vec3 + 2 * 4]   // Vec3[2]
  movss xmm2, [Coef22]    // Coef22
  mulss xmm1, xmm2
  addss xmm0, xmm1
  movss dword ptr [Inv2 + 2 * 4], xmm0   // Inv2[2]

  // Inv2[3] = - (Vec0[3] * Coef07 - Vec1[3] * Coef15 + Vec3[3] * Coef23)
  movss xmm0, dword ptr [Vec0 + 3 * 4]    // Vec0[3]
  movss xmm1, [Coef07]    // Coef07
  mulss xmm0, xmm1
  movss xmm1, dword ptr [Vec1 + 3 * 4]   // Vec1[3]
  movss xmm2, [Coef15]    // Coef15
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, dword ptr [Vec3 + 3 * 4]   // Vec3[3]
  movss xmm2, [Coef23]    // Coef23
  mulss xmm1, xmm2
  addss xmm0, xmm1
  xorps xmm1, xmm1
  subss xmm1, xmm0        // Negate
  movss dword ptr [Inv2 + 3 * 4], xmm1   // Inv2[3]

  // ============================================================================
  // Calculate Inv3
  // ============================================================================

  // Inv3[0] = - (Vec0[0] * Coef08 - Vec1[0] * Coef16 + Vec2[0] * Coef20)
  movss xmm0, dword ptr [Vec0 + 0]    // Vec0[0]
  movss xmm1, [Coef08]    // Coef08
  mulss xmm0, xmm1
  movss xmm1, dword ptr [Vec1 + 0]    // Vec1[0]
  movss xmm2, [Coef16]    // Coef16
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, dword ptr [Vec2 + 0]   // Vec2[0]
  movss xmm2, [Coef20]    // Coef20
  mulss xmm1, xmm2
  addss xmm0, xmm1
  xorps xmm1, xmm1
  subss xmm1, xmm0        // Negate
  movss dword ptr [Inv3 + 0], xmm1   // Inv3[0]

  // Inv3[1] = + (Vec0[1] * Coef08 - Vec1[1] * Coef16 + Vec2[1] * Coef20)
  movss xmm0, dword ptr [Vec0 + 1 * 4]    // Vec0[1]
  movss xmm1, [Coef08]    // Coef08
  mulss xmm0, xmm1
  movss xmm1, dword ptr [Vec1 + 1 * 4]   // Vec1[1]
  movss xmm2, [Coef16]    // Coef16
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, dword ptr [Vec2 + 1 * 4]   // Vec2[1]
  movss xmm2, [Coef20]    // Coef20
  mulss xmm1, xmm2
  addss xmm0, xmm1
  movss dword ptr [Inv3 + 1 * 4], xmm0   // Inv3[1]

  // Inv3[2] = - (Vec0[2] * Coef10 - Vec1[2] * Coef18 + Vec2[2] * Coef22)
  movss xmm0, dword ptr [Vec0 + 2 * 4]    // Vec0[2]
  movss xmm1, [Coef10]    // Coef10
  mulss xmm0, xmm1
  movss xmm1, dword ptr [Vec1 + 2 * 4]   // Vec1[2]
  movss xmm2, [Coef18]    // Coef18
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, dword ptr [Vec2 + 2 * 4]   // Vec2[2]
  movss xmm2, [Coef22]    // Coef22
  mulss xmm1, xmm2
  addss xmm0, xmm1
  xorps xmm1, xmm1
  subss xmm1, xmm0        // Negate
  movss dword ptr [Inv3 + 2 * 4], xmm1   // Inv3[2]

  // Inv3[3] = + (Vec0[3] * Coef11 - Vec1[3] * Coef19 + Vec2[3] * Coef23)
  movss xmm0, dword ptr [Vec0 + 3 * 4]    // Vec0[3]
  movss xmm1, [Coef11]    // Coef11
  mulss xmm0, xmm1
  movss xmm1, dword ptr [Vec1 + 3 * 4]   // Vec1[3]
  movss xmm2, [Coef19]    // Coef19
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, dword ptr [Vec2 + 3 * 4]   // Vec2[3]
  movss xmm2, [Coef23]    // Coef23
  mulss xmm1, xmm2
  addss xmm0, xmm1
  movss dword ptr [Inv3 + 3 * 4], xmm0   // Inv3[3]

  // ============================================================================
  // Build Row0 and calculate determinant
  // Row0 = [Inv0[0], Inv1[0], Inv2[0], Inv3[0]]
  // Dot0 = m[0,0]*Row0[0] + m[0,1]*Row0[1] + m[0,2]*Row0[2] + m[0,3]*Row0[3]
  // ============================================================================

  movss xmm0, dword ptr [Inv0 + 0]   // Inv0[0]
  movss xmm1, dword ptr [Inv1 + 0]   // Inv1[0]
  movss xmm2, dword ptr [Inv2 + 0]   // Inv2[0]
  movss xmm3, dword ptr [Inv3 + 0]   // Inv3[0]
  unpcklps xmm0, xmm1     // [Inv0[0], Inv1[0], 0, 0]
  unpcklps xmm2, xmm3     // [Inv2[0], Inv3[0], 0, 0]
  movlhps xmm0, xmm2      // Row0
  movups [Row0], xmm0

  // Calculate determinant using SIMD dot product
  movaps xmm1, [rax]      // Load m[0,*]
  mulps xmm0, xmm1        // Component-wise multiply

  // Horizontal add
  movaps xmm1, xmm0
  shufps xmm1, xmm1, $4E // Swap high and low
  addps xmm0, xmm1
  movaps xmm1, xmm0
  shufps xmm1, xmm1, $11
  addps xmm0, xmm1        // xmm0[0] = Dot0

  // Calculate 1/Dot0 and broadcast
  movss xmm1, [rip+One]
  divss xmm1, xmm0
  shufps xmm1, xmm1, 0    // Broadcast InvDet to all 4 components

  // ============================================================================
  // Multiply all Inv vectors by InvDet and store result
  // ============================================================================

  movups xmm0, [Inv0]  // Inv0
  mulps xmm0, xmm1
  movaps [rdi], xmm0      // Result row 0

  movups xmm0, [Inv1]  // Inv1
  mulps xmm0, xmm1
  movaps [rdi+16], xmm0   // Result row 1

  movups xmm0, [Inv2]  // Inv2
  mulps xmm0, xmm1
  movaps [rdi+32], xmm0   // Result row 2

  movups xmm0, [Inv3]  // Inv3
  mulps xmm0, xmm1
  movaps [rdi+48], xmm0   // Result row 3
end;

function UMatInverse_SSE2(const m: TUMat): TUMat; assembler;
  const One: Single = 1;
  const SignMask: array[0..3] of UInt32 = ($80000000, $80000000, $80000000, $80000000);
  var Coef06, Coef07: Single;
  var Coef08, Coef10, Coef11: Single;
  var Coef12, Coef14, Coef15: Single;
  var Coef16, Coef18, Coef19: Single;
  var Coef20, Coef22, Coef23: Single;
asm
  mov rax, m
  mov rdx, Result

  // ============================================================================
  // Pre-load frequently used matrix values into high XMM registers
  // ============================================================================

  movss xmm8, [rax+60]     // m[3,3]
  movss xmm9, [rax+56]     // m[3,2]
  movss xmm10, [rax+52]    // m[3,1]
  movss xmm11, [rax+48]    // m[3,0]

  // ============================================================================
  // Calculate all Coef values
  // Store Coef00, Coef02, Coef03, Coef04 in registers
  // Rest go to stack
  // ============================================================================

  // Coef00 = m[2,2] * m[3,3] - m[3,2] * m[2,3]
  movss xmm0, [rax+40]
  mulss xmm0, xmm8
  movss xmm1, xmm9
  movss xmm2, [rax+44]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm4, xmm0         // Save Coef00 in xmm4

  // Coef02 = m[1,2] * m[3,3] - m[3,2] * m[1,3]
  movss xmm0, [rax+24]
  mulss xmm0, xmm8
  movss xmm1, xmm9
  movss xmm2, [rax+28]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm5, xmm0         // Save Coef02 in xmm5

  // Coef03 = m[1,2] * m[2,3] - m[2,2] * m[1,3]
  movss xmm0, [rax+24]
  movss xmm1, [rax+44]
  mulss xmm0, xmm1
  movss xmm1, [rax+40]
  movss xmm2, [rax+28]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm6, xmm0         // Save Coef03 in xmm6

  // Coef04 = m[2,1] * m[3,3] - m[3,1] * m[2,3]
  movss xmm0, [rax+36]
  mulss xmm0, xmm8
  movss xmm1, xmm10
  movss xmm2, [rax+44]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm7, xmm0         // Save Coef04 in xmm7

  // Coef06 = m[1,1] * m[3,3] - m[3,1] * m[1,3]
  movss xmm0, [rax+20]
  mulss xmm0, xmm8
  movss xmm1, xmm10
  movss xmm2, [rax+28]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef06], xmm0

  // Coef07 = m[1,1] * m[2,3] - m[2,1] * m[1,3]
  movss xmm0, [rax+20]
  movss xmm1, [rax+44]
  mulss xmm0, xmm1
  movss xmm1, [rax+36]
  movss xmm2, [rax+28]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef07], xmm0

  // Coef08 = m[2,1] * m[3,2] - m[3,1] * m[2,2]
  movss xmm0, [rax+36]
  mulss xmm0, xmm9
  movss xmm1, xmm10
  movss xmm2, [rax+40]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef08], xmm0

  // Coef10 = m[1,1] * m[3,2] - m[3,1] * m[1,2]
  movss xmm0, [rax+20]
  mulss xmm0, xmm9
  movss xmm1, xmm10
  movss xmm2, [rax+24]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef10], xmm0

  // Coef11 = m[1,1] * m[2,2] - m[2,1] * m[1,2]
  movss xmm0, [rax+20]
  movss xmm1, [rax+40]
  mulss xmm0, xmm1
  movss xmm1, [rax+36]
  movss xmm2, [rax+24]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef11], xmm0

  // Coef12 = m[2,0] * m[3,3] - m[3,0] * m[2,3]
  movss xmm0, [rax+32]
  mulss xmm0, xmm8
  movss xmm1, xmm11
  movss xmm2, [rax+44]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef12], xmm0

  // Coef14 = m[1,0] * m[3,3] - m[3,0] * m[1,3]
  movss xmm0, [rax+16]
  mulss xmm0, xmm8
  movss xmm1, xmm11
  movss xmm2, [rax+28]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef14], xmm0

  // Coef15 = m[1,0] * m[2,3] - m[2,0] * m[1,3]
  movss xmm0, [rax+16]
  movss xmm1, [rax+44]
  mulss xmm0, xmm1
  movss xmm1, [rax+32]
  movss xmm2, [rax+28]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef15], xmm0

  // Coef16 = m[2,0] * m[3,2] - m[3,0] * m[2,2]
  movss xmm0, [rax+32]
  mulss xmm0, xmm9
  movss xmm1, xmm11
  movss xmm2, [rax+40]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef16], xmm0

  // Coef18 = m[1,0] * m[3,2] - m[3,0] * m[1,2]
  movss xmm0, [rax+16]
  mulss xmm0, xmm9
  movss xmm1, xmm11
  movss xmm2, [rax+24]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef18], xmm0

  // Coef19 = m[1,0] * m[2,2] - m[2,0] * m[1,2]
  movss xmm0, [rax+16]
  movss xmm1, [rax+40]
  mulss xmm0, xmm1
  movss xmm1, [rax+32]
  movss xmm2, [rax+24]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef19], xmm0

  // Coef20 = m[2,0] * m[3,1] - m[3,0] * m[2,1]
  movss xmm0, [rax+32]
  mulss xmm0, xmm10
  movss xmm1, xmm11
  movss xmm2, [rax+36]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef20], xmm0

  // Coef22 = m[1,0] * m[3,1] - m[3,0] * m[1,1]
  movss xmm0, [rax+16]
  mulss xmm0, xmm10
  movss xmm1, xmm11
  movss xmm2, [rax+20]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef22], xmm0

  // Coef23 = m[1,0] * m[2,1] - m[2,0] * m[1,1]
  movss xmm0, [rax+16]
  movss xmm1, [rax+36]
  mulss xmm0, xmm1
  movss xmm1, [rax+32]
  movss xmm2, [rax+20]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef23], xmm0

  // ============================================================================
  // Build Vec0, Vec1, Vec2, Vec3 - FIXED VEraxON
  // ============================================================================

  // Vec0 = [m[1,0], m[0,0], m[0,0], m[0,0]]
  movss xmm0, [rax+16]     // m[1,0]
  movss xmm1, [rax]        // m[0,0]
  unpcklps xmm0, xmm1      // [m[1,0], m[0,0], 0, 0]
  movss xmm2, [rax]
  movss xmm3, [rax]
  unpcklps xmm2, xmm3      // [m[0,0], m[0,0], 0, 0]
  movlhps xmm0, xmm2       // [m[1,0], m[0,0], m[0,0], m[0,0]]
  movaps xmm12, xmm0       // Save Vec0 in xmm12

  // Vec1 = [m[1,1], m[0,1], m[0,1], m[0,1]]
  movss xmm0, [rax+20]     // m[1,1]
  movss xmm1, [rax+4]      // m[0,1]
  unpcklps xmm0, xmm1
  movss xmm2, [rax+4]
  movss xmm3, [rax+4]
  unpcklps xmm2, xmm3
  movlhps xmm0, xmm2
  movaps xmm13, xmm0       // Save Vec1 in xmm13

  // Vec2 = [m[1,2], m[0,2], m[0,2], m[0,2]]
  movss xmm0, [rax+24]     // m[1,2]
  movss xmm1, [rax+8]      // m[0,2]
  unpcklps xmm0, xmm1
  movss xmm2, [rax+8]
  movss xmm3, [rax+8]
  unpcklps xmm2, xmm3
  movlhps xmm0, xmm2
  movaps xmm14, xmm0       // Save Vec2 in xmm14

  // Vec3 = [m[1,3], m[0,3], m[0,3], m[0,3]]
  movss xmm0, [rax+28]     // m[1,3]
  movss xmm1, [rax+12]     // m[0,3]
  unpcklps xmm0, xmm1
  movss xmm2, [rax+12]
  movss xmm3, [rax+12]
  unpcklps xmm2, xmm3
  movlhps xmm0, xmm2
  movaps xmm15, xmm0       // Save Vec3 in xmm15

  // ============================================================================
  // Calculate Inv rows and write directly to output
  // ============================================================================

  movaps xmm3, [rip+SignMask]  // Load sign mask

  // Inv0[0] = + (Vec1[0] * Coef00 - Vec2[0] * Coef04 + Vec3[0] * Coef08)
  movss xmm0, xmm13
  mulss xmm0, xmm4
  movss xmm1, xmm14
  mulss xmm1, xmm7
  subss xmm0, xmm1
  movss xmm1, xmm15
  mulss xmm1, [Coef08]
  addss xmm0, xmm1
  movss [rdx], xmm0

  // Inv0[1] = - (Vec1[1] * Coef00 - Vec2[1] * Coef04 + Vec3[1] * Coef08)
  movaps xmm0, xmm13
  shufps xmm0, xmm0, $55
  mulss xmm0, xmm4
  movaps xmm1, xmm14
  shufps xmm1, xmm1, $55
  mulss xmm1, xmm7
  subss xmm0, xmm1
  movaps xmm1, xmm15
  shufps xmm1, xmm1, $55
  mulss xmm1, [Coef08]
  addss xmm0, xmm1
  xorps xmm0, xmm3
  movss [rdx+4], xmm0

  // Inv0[2] = + (Vec1[2] * Coef02 - Vec2[2] * Coef06 + Vec3[2] * Coef10)
  movaps xmm0, xmm13
  shufps xmm0, xmm0, $AA
  mulss xmm0, xmm5
  movaps xmm1, xmm14
  shufps xmm1, xmm1, $AA
  mulss xmm1, [Coef06]
  subss xmm0, xmm1
  movaps xmm1, xmm15
  shufps xmm1, xmm1, $AA
  mulss xmm1, [Coef10]
  addss xmm0, xmm1
  movss [rdx+8], xmm0

  // Inv0[3] = - (Vec1[3] * Coef03 - Vec2[3] * Coef07 + Vec3[3] * Coef11)
  movaps xmm0, xmm13
  shufps xmm0, xmm0, $FF
  mulss xmm0, xmm6
  movaps xmm1, xmm14
  shufps xmm1, xmm1, $FF
  mulss xmm1, [Coef07]
  subss xmm0, xmm1
  movaps xmm1, xmm15
  shufps xmm1, xmm1, $FF
  mulss xmm1, [Coef11]
  addss xmm0, xmm1
  xorps xmm0, xmm3
  movss [rdx+12], xmm0

  // ============================================================================
  // Inv1
  // ============================================================================

  // Inv1[0] = - (Vec0[0] * Coef00 - Vec2[0] * Coef12 + Vec3[0] * Coef16)
  movss xmm0, xmm12
  mulss xmm0, xmm4
  movss xmm1, xmm14
  mulss xmm1, [Coef12]
  subss xmm0, xmm1
  movss xmm1, xmm15
  mulss xmm1, [Coef16]
  addss xmm0, xmm1
  xorps xmm0, xmm3
  movss [rdx+16], xmm0

  // Inv1[1] = + (Vec0[1] * Coef00 - Vec2[1] * Coef12 + Vec3[1] * Coef16)
  movaps xmm0, xmm12
  shufps xmm0, xmm0, $55
  mulss xmm0, xmm4
  movaps xmm1, xmm14
  shufps xmm1, xmm1, $55
  mulss xmm1, [Coef12]
  subss xmm0, xmm1
  movaps xmm1, xmm15
  shufps xmm1, xmm1, $55
  mulss xmm1, [Coef16]
  addss xmm0, xmm1
  movss [rdx+20], xmm0

  // Inv1[2] = - (Vec0[2] * Coef02 - Vec2[2] * Coef14 + Vec3[2] * Coef18)
  movaps xmm0, xmm12
  shufps xmm0, xmm0, $AA
  mulss xmm0, xmm5
  movaps xmm1, xmm14
  shufps xmm1, xmm1, $AA
  mulss xmm1, [Coef14]
  subss xmm0, xmm1
  movaps xmm1, xmm15
  shufps xmm1, xmm1, $AA
  mulss xmm1, [Coef18]
  addss xmm0, xmm1
  xorps xmm0, xmm3
  movss [rdx+24], xmm0

  // Inv1[3] = + (Vec0[3] * Coef03 - Vec2[3] * Coef15 + Vec3[3] * Coef19)
  movaps xmm0, xmm12
  shufps xmm0, xmm0, $FF
  mulss xmm0, xmm6
  movaps xmm1, xmm14
  shufps xmm1, xmm1, $FF
  mulss xmm1, [Coef15]
  subss xmm0, xmm1
  movaps xmm1, xmm15
  shufps xmm1, xmm1, $FF
  mulss xmm1, [Coef19]
  addss xmm0, xmm1
  movss [rdx+28], xmm0

  // ============================================================================
  // Inv2
  // ============================================================================

  // Inv2[0] = + (Vec0[0] * Coef04 - Vec1[0] * Coef12 + Vec3[0] * Coef20)
  movss xmm0, xmm12
  mulss xmm0, xmm7
  movss xmm1, xmm13
  mulss xmm1, [Coef12]
  subss xmm0, xmm1
  movss xmm1, xmm15
  mulss xmm1, [Coef20]
  addss xmm0, xmm1
  movss [rdx+32], xmm0

  // Inv2[1] = - (Vec0[1] * Coef04 - Vec1[1] * Coef12 + Vec3[1] * Coef20)
  movaps xmm0, xmm12
  shufps xmm0, xmm0, $55
  mulss xmm0, xmm7
  movaps xmm1, xmm13
  shufps xmm1, xmm1, $55
  mulss xmm1, [Coef12]
  subss xmm0, xmm1
  movaps xmm1, xmm15
  shufps xmm1, xmm1, $55
  mulss xmm1, [Coef20]
  addss xmm0, xmm1
  xorps xmm0, xmm3
  movss [rdx+36], xmm0

  // Inv2[2] = + (Vec0[2] * Coef06 - Vec1[2] * Coef14 + Vec3[2] * Coef22)
  movaps xmm0, xmm12
  shufps xmm0, xmm0, $AA
  mulss xmm0, [Coef06]
  movaps xmm1, xmm13
  shufps xmm1, xmm1, $AA
  mulss xmm1, [Coef14]
  subss xmm0, xmm1
  movaps xmm1, xmm15
  shufps xmm1, xmm1, $AA
  mulss xmm1, [Coef22]
  addss xmm0, xmm1
  movss [rdx+40], xmm0

  // Inv2[3] = - (Vec0[3] * Coef07 - Vec1[3] * Coef15 + Vec3[3] * Coef23)
  movaps xmm0, xmm12
  shufps xmm0, xmm0, $FF
  mulss xmm0, [Coef07]
  movaps xmm1, xmm13
  shufps xmm1, xmm1, $FF
  mulss xmm1, [Coef15]
  subss xmm0, xmm1
  movaps xmm1, xmm15
  shufps xmm1, xmm1, $FF
  mulss xmm1, [Coef23]
  addss xmm0, xmm1
  xorps xmm0, xmm3
  movss [rdx+44], xmm0

  // ============================================================================
  // Inv3
  // ============================================================================

  // Inv3[0] = - (Vec0[0] * Coef08 - Vec1[0] * Coef16 + Vec2[0] * Coef20)
  movss xmm0, xmm12
  mulss xmm0, [Coef08]
  movss xmm1, xmm13
  mulss xmm1, [Coef16]
  subss xmm0, xmm1
  movss xmm1, xmm14
  mulss xmm1, [Coef20]
  addss xmm0, xmm1
  xorps xmm0, xmm3
  movss [rdx+48], xmm0

  // Inv3[1] = + (Vec0[1] * Coef08 - Vec1[1] * Coef16 + Vec2[1] * Coef20)
  movaps xmm0, xmm12
  shufps xmm0, xmm0, $55
  mulss xmm0, [Coef08]
  movaps xmm1, xmm13
  shufps xmm1, xmm1, $55
  mulss xmm1, [Coef16]
  subss xmm0, xmm1
  movaps xmm1, xmm14
  shufps xmm1, xmm1, $55
  mulss xmm1, [Coef20]
  addss xmm0, xmm1
  movss [rdx+52], xmm0

  // Inv3[2] = - (Vec0[2] * Coef10 - Vec1[2] * Coef18 + Vec2[2] * Coef22)
  movaps xmm0, xmm12
  shufps xmm0, xmm0, $AA
  mulss xmm0, [Coef10]
  movaps xmm1, xmm13
  shufps xmm1, xmm1, $AA
  mulss xmm1, [Coef18]
  subss xmm0, xmm1
  movaps xmm1, xmm14
  shufps xmm1, xmm1, $AA
  mulss xmm1, [Coef22]
  addss xmm0, xmm1
  xorps xmm0, xmm3
  movss [rdx+56], xmm0

  // Inv3[3] = + (Vec0[3] * Coef11 - Vec1[3] * Coef19 + Vec2[3] * Coef23)
  movaps xmm0, xmm12
  shufps xmm0, xmm0, $FF
  mulss xmm0, [Coef11]
  movaps xmm1, xmm13
  shufps xmm1, xmm1, $FF
  mulss xmm1, [Coef19]
  subss xmm0, xmm1
  movaps xmm1, xmm14
  shufps xmm1, xmm1, $FF
  mulss xmm1, [Coef23]
  addss xmm0, xmm1
  movss [rdx+60], xmm0

  // ============================================================================
  // Calculate determinant and divide
  // ============================================================================

  // Build Row0 from first elements of each Inv row
  movss xmm0, [rdx]
  movss xmm1, [rdx+16]
  movss xmm2, [rdx+32]
  movss xmm3, [rdx+48]
  unpcklps xmm0, xmm1
  unpcklps xmm2, xmm3
  movlhps xmm0, xmm2

  // Dot product with m[0,*]
  movaps xmm1, [rax]
  mulps xmm0, xmm1

  // Horizontal sum
  movaps xmm1, xmm0
  shufps xmm1, xmm1, $4E
  addps xmm0, xmm1
  movaps xmm1, xmm0
  shufps xmm1, xmm1, $11
  addps xmm0, xmm1

  // InvDet = 1 / Dot0
  movss xmm1, [rip+One]
  divss xmm1, xmm0
  shufps xmm1, xmm1, 0

  // Multiply all results by InvDet
  movaps xmm0, [rdx]
  mulps xmm0, xmm1
  movaps [rdx], xmm0

  movaps xmm0, [rdx+16]
  mulps xmm0, xmm1
  movaps [rdx+16], xmm0

  movaps xmm0, [rdx+32]
  mulps xmm0, xmm1
  movaps [rdx+32], xmm0

  movaps xmm0, [rdx+48]
  mulps xmm0, xmm1
  movaps [rdx+48], xmm0
end;

procedure Test(const Vec4: PUVec4);
  const ConstOne: TUFloat = 1;
begin
  Vec4^[0] := ConstOne;
  Vec4^[1] := ConstOne;
  Vec4^[2] := ConstOne;
  Vec4^[3] := ConstOne;
end;

procedure TestAsm(const Vec4: PUVec4); assembler;
  const ConstOne: TUFloat = 1;
asm
  movss xmm0, [rip + ConstOne]
  shufps xmm0, xmm0, 0
  movaps dqword ptr [Vec4], xmm0
end;

procedure Test2(var n: UInt32);
  const One: TUFloat = 1;
begin
  n := PUInt32(@One)^;
end;

function UMatInverse_Pas2(const m: TUMat): TUMat;
var
  Coef00, Coef02, Coef03: Single;
  Coef04, Coef06, Coef07: Single;
  Coef08, Coef10, Coef11: Single;
  Coef12, Coef14, Coef15: Single;
  Coef16, Coef18, Coef19: Single;
  Coef20, Coef22, Coef23: Single;

  Vec0, Vec1, Vec2, Vec3: TUVec4;
  Inv0, Inv1, Inv2, Inv3: TUVec4;
  Row0: TUVec4;
  Dot0: Single;
  InvDet: Single;
begin
  Coef00 := m[2, 2] * m[3, 3] - m[3, 2] * m[2, 3];
  Coef04 := m[2, 1] * m[3, 3] - m[3, 1] * m[2, 3];
  Coef08 := m[2, 1] * m[3, 2] - m[3, 1] * m[2, 2];
  Coef12 := m[2, 0] * m[3, 3] - m[3, 0] * m[2, 3];
  Coef16 := m[2, 0] * m[3, 2] - m[3, 0] * m[2, 2];
  Coef20 := m[2, 0] * m[3, 1] - m[3, 0] * m[2, 1];

  Vec0[0] := m[1, 0];
  Vec1[0] := m[1, 1];
  Vec2[0] := m[1, 2];
  Vec3[0] := m[1, 3];

  Inv0[0] := + (Vec1[0] * Coef00 - Vec2[0] * Coef04 + Vec3[0] * Coef08);
  Inv1[0] := - (Vec0[0] * Coef00 - Vec2[0] * Coef12 + Vec3[0] * Coef16);
  Inv2[0] := + (Vec0[0] * Coef04 - Vec1[0] * Coef12 + Vec3[0] * Coef20);
  Inv3[0] := - (Vec0[0] * Coef08 - Vec1[0] * Coef16 + Vec2[0] * Coef20);

  Row0[0] := Inv0[0];
  Row0[1] := Inv1[0];
  Row0[2] := Inv2[0];
  Row0[3] := Inv3[0];

  Dot0 := m[0, 0] * Row0[0] + m[0, 1] * Row0[1] + m[0, 2] * Row0[2] + m[0, 3] * Row0[3];
  InvDet := 1.0 / Dot0;

  Coef02 := m[1, 2] * m[3, 3] - m[3, 2] * m[1, 3];
  Coef03 := m[1, 2] * m[2, 3] - m[2, 2] * m[1, 3];
  Coef06 := m[1, 1] * m[3, 3] - m[3, 1] * m[1, 3];
  Coef07 := m[1, 1] * m[2, 3] - m[2, 1] * m[1, 3];
  Coef10 := m[1, 1] * m[3, 2] - m[3, 1] * m[1, 2];
  Coef11 := m[1, 1] * m[2, 2] - m[2, 1] * m[1, 2];

  Vec1[1] := m[0, 1];
  Vec1[2] := m[0, 1];
  Vec1[3] := m[0, 1];

  Vec2[1] := m[0, 2];
  Vec2[2] := m[0, 2];
  Vec2[3] := m[0, 2];

  Vec3[1] := m[0, 3];
  Vec3[2] := m[0, 3];
  Vec3[3] := m[0, 3];

  Inv0[1] := - (Vec1[1] * Coef00 - Vec2[1] * Coef04 + Vec3[1] * Coef08);
  Inv0[2] := + (Vec1[2] * Coef02 - Vec2[2] * Coef06 + Vec3[2] * Coef10);
  Inv0[3] := - (Vec1[3] * Coef03 - Vec2[3] * Coef07 + Vec3[3] * Coef11);

  Result[0, 0] := Inv0[0] * InvDet;
  Result[0, 1] := Inv0[1] * InvDet;
  Result[0, 2] := Inv0[2] * InvDet;
  Result[0, 3] := Inv0[3] * InvDet;

  Coef14 := m[1, 0] * m[3, 3] - m[3, 0] * m[1, 3];
  Coef15 := m[1, 0] * m[2, 3] - m[2, 0] * m[1, 3];
  Coef18 := m[1, 0] * m[3, 2] - m[3, 0] * m[1, 2];
  Coef19 := m[1, 0] * m[2, 2] - m[2, 0] * m[1, 2];

  Inv1[1] := + (Vec0[1] * Coef00 - Vec2[1] * Coef12 + Vec3[1] * Coef16);
  Inv1[2] := - (Vec0[2] * Coef02 - Vec2[2] * Coef14 + Vec3[2] * Coef18);
  Inv1[3] := + (Vec0[3] * Coef03 - Vec2[3] * Coef15 + Vec3[3] * Coef19);

  Result[1, 0] := Inv1[0] * InvDet;
  Result[1, 1] := Inv1[1] * InvDet;
  Result[1, 2] := Inv1[2] * InvDet;
  Result[1, 3] := Inv1[3] * InvDet;

  Coef22 := m[1, 0] * m[3, 1] - m[3, 0] * m[1, 1];
  Coef23 := m[1, 0] * m[2, 1] - m[2, 0] * m[1, 1];

  Vec0[1] := m[0, 0];
  Vec0[2] := m[0, 0];
  Vec0[3] := m[0, 0];

  Inv2[1] := - (Vec0[1] * Coef04 - Vec1[1] * Coef12 + Vec3[1] * Coef20);
  Inv2[2] := + (Vec0[2] * Coef06 - Vec1[2] * Coef14 + Vec3[2] * Coef22);
  Inv2[3] := - (Vec0[3] * Coef07 - Vec1[3] * Coef15 + Vec3[3] * Coef23);

  Result[2, 0] := Inv2[0] * InvDet;
  Result[2, 1] := Inv2[1] * InvDet;
  Result[2, 2] := Inv2[2] * InvDet;
  Result[2, 3] := Inv2[3] * InvDet;

  Inv3[1] := + (Vec0[1] * Coef08 - Vec1[1] * Coef16 + Vec2[1] * Coef20);
  Inv3[2] := - (Vec0[2] * Coef10 - Vec1[2] * Coef18 + Vec2[2] * Coef22);
  Inv3[3] := + (Vec0[3] * Coef11 - Vec1[3] * Coef19 + Vec2[3] * Coef23);

  Result[3, 0] := Inv3[0] * InvDet;
  Result[3, 1] := Inv3[1] * InvDet;
  Result[3, 2] := Inv3[2] * InvDet;
  Result[3, 3] := Inv3[3] * InvDet;
end;

procedure Run;
  var v: TUVec4;
  var m, m1, m2, mt: TUMat;
  var n: UInt32;
begin
  n := 0;
  Test2(n);
  WriteLn(IntToHex(n));
  m := TUMat.RotationY(Pi * 0.25) * TUMat.RotationZ(Pi * 0.3) * TUMat.RotationX(Pi * 0.1);
  WriteLn('m: ', m.ToString);
  m1 := UMatInverse(m);
  WriteLn('m1: ', m1.ToString);
  //m2 := UMatInverse(m1);
  //WriteLn('m2: ', m2.ToString);
  //mt := UMatInverse_SSE2(m);
  //mt := UMatInverse_SSE2(m);
  mt := UMatInverse_Pas2(m);
  WriteLn('mt: ', mt.ToString);
  //Test(@v);
  //TestAsm(@v);
  //WriteLn('v: ', v.ToString);
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

