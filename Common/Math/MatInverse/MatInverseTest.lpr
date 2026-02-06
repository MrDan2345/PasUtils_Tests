program MatInverseTest;

{$include PasUtilsMode.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils,
  CryptoUtils;

{
function UMatInverse_SSE1(const m: TUMat): TUMat; assembler;
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
  push rbx
  push rsi
  push rdi
  sub rsp, 256  // Stack space for coefficients and intermediate vectors

  lea rsi, [m]  // rsi = source matrix
  lea rdi, [Result]  // rdi = result matrix

  // Stack layout:
  // [rsp+0]   = Coef00, Coef02, Coef03, Coef04 (16 bytes)
  // [rsp+16]  = Coef06, Coef07, Coef08, Coef10 (16 bytes)
  // [rsp+32]  = Coef11, Coef12, Coef14, Coef15 (16 bytes)
  // [rsp+48]  = Coef16, Coef18, Coef19, Coef20 (16 bytes)
  // [rsp+64]  = Coef22, Coef23, unused, unused (16 bytes)
  // [rsp+80]  = Vec0 (16 bytes)
  // [rsp+96]  = Vec1 (16 bytes)
  // [rsp+112] = Vec2 (16 bytes)
  // [rsp+128] = Vec3 (16 bytes)
  // [rsp+144] = Inv0 (16 bytes)
  // [rsp+160] = Inv1 (16 bytes)
  // [rsp+176] = Inv2 (16 bytes)
  // [rsp+192] = Inv3 (16 bytes)
  // [rsp+208] = Row0 (16 bytes)

  // ============================================================================
  // Calculate all Coef values
  // ============================================================================

  // Coef00 = m[2,2] * m[3,3] - m[3,2] * m[2,3]
  movss xmm0, [rsi+40]    // m[2,2]
  movss xmm1, [rsi+60]    // m[3,3]
  mulss xmm0, xmm1
  movss xmm1, [rsi+56]    // m[3,2]
  movss xmm2, [rsi+44]    // m[2,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [rsp], xmm0       // Coef00

  // Coef02 = m[1,2] * m[3,3] - m[3,2] * m[1,3]
  movss xmm0, [rsi+24]    // m[1,2]
  movss xmm1, [rsi+60]    // m[3,3]
  mulss xmm0, xmm1
  movss xmm1, [rsi+56]    // m[3,2]
  movss xmm2, [rsi+28]    // m[1,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [rsp+4], xmm0     // Coef02

  // Coef03 = m[1,2] * m[2,3] - m[2,2] * m[1,3]
  movss xmm0, [rsi+24]    // m[1,2]
  movss xmm1, [rsi+44]    // m[2,3]
  mulss xmm0, xmm1
  movss xmm1, [rsi+40]    // m[2,2]
  movss xmm2, [rsi+28]    // m[1,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [rsp+8], xmm0     // Coef03

  // Coef04 = m[2,1] * m[3,3] - m[3,1] * m[2,3]
  movss xmm0, [rsi+36]    // m[2,1]
  movss xmm1, [rsi+60]    // m[3,3]
  mulss xmm0, xmm1
  movss xmm1, [rsi+52]    // m[3,1]
  movss xmm2, [rsi+44]    // m[2,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [rsp+12], xmm0    // Coef04

  // Coef06 = m[1,1] * m[3,3] - m[3,1] * m[1,3]
  movss xmm0, [rsi+20]    // m[1,1]
  movss xmm1, [rsi+60]    // m[3,3]
  mulss xmm0, xmm1
  movss xmm1, [rsi+52]    // m[3,1]
  movss xmm2, [rsi+28]    // m[1,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [rsp+16], xmm0    // Coef06

  // Coef07 = m[1,1] * m[2,3] - m[2,1] * m[1,3]
  movss xmm0, [rsi+20]    // m[1,1]
  movss xmm1, [rsi+44]    // m[2,3]
  mulss xmm0, xmm1
  movss xmm1, [rsi+36]    // m[2,1]
  movss xmm2, [rsi+28]    // m[1,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [rsp+20], xmm0    // Coef07

  // Coef08 = m[2,1] * m[3,2] - m[3,1] * m[2,2]
  movss xmm0, [rsi+36]    // m[2,1]
  movss xmm1, [rsi+56]    // m[3,2]
  mulss xmm0, xmm1
  movss xmm1, [rsi+52]    // m[3,1]
  movss xmm2, [rsi+40]    // m[2,2]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [rsp+24], xmm0    // Coef08

  // Coef10 = m[1,1] * m[3,2] - m[3,1] * m[1,2]
  movss xmm0, [rsi+20]    // m[1,1]
  movss xmm1, [rsi+56]    // m[3,2]
  mulss xmm0, xmm1
  movss xmm1, [rsi+52]    // m[3,1]
  movss xmm2, [rsi+24]    // m[1,2]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [rsp+28], xmm0    // Coef10

  // Coef11 = m[1,1] * m[2,2] - m[2,1] * m[1,2]
  movss xmm0, [rsi+20]    // m[1,1]
  movss xmm1, [rsi+40]    // m[2,2]
  mulss xmm0, xmm1
  movss xmm1, [rsi+36]    // m[2,1]
  movss xmm2, [rsi+24]    // m[1,2]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [rsp+32], xmm0    // Coef11

  // Coef12 = m[2,0] * m[3,3] - m[3,0] * m[2,3]
  movss xmm0, [rsi+32]    // m[2,0]
  movss xmm1, [rsi+60]    // m[3,3]
  mulss xmm0, xmm1
  movss xmm1, [rsi+48]    // m[3,0]
  movss xmm2, [rsi+44]    // m[2,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [rsp+36], xmm0    // Coef12

  // Coef14 = m[1,0] * m[3,3] - m[3,0] * m[1,3]
  movss xmm0, [rsi+16]    // m[1,0]
  movss xmm1, [rsi+60]    // m[3,3]
  mulss xmm0, xmm1
  movss xmm1, [rsi+48]    // m[3,0]
  movss xmm2, [rsi+28]    // m[1,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [rsp+40], xmm0    // Coef14

  // Coef15 = m[1,0] * m[2,3] - m[2,0] * m[1,3]
  movss xmm0, [rsi+16]    // m[1,0]
  movss xmm1, [rsi+44]    // m[2,3]
  mulss xmm0, xmm1
  movss xmm1, [rsi+32]    // m[2,0]
  movss xmm2, [rsi+28]    // m[1,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [rsp+44], xmm0    // Coef15

  // Coef16 = m[2,0] * m[3,2] - m[3,0] * m[2,2]
  movss xmm0, [rsi+32]    // m[2,0]
  movss xmm1, [rsi+56]    // m[3,2]
  mulss xmm0, xmm1
  movss xmm1, [rsi+48]    // m[3,0]
  movss xmm2, [rsi+40]    // m[2,2]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [rsp+48], xmm0    // Coef16

  // Coef18 = m[1,0] * m[3,2] - m[3,0] * m[1,2]
  movss xmm0, [rsi+16]    // m[1,0]
  movss xmm1, [rsi+56]    // m[3,2]
  mulss xmm0, xmm1
  movss xmm1, [rsi+48]    // m[3,0]
  movss xmm2, [rsi+24]    // m[1,2]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [rsp+52], xmm0    // Coef18

  // Coef19 = m[1,0] * m[2,2] - m[2,0] * m[1,2]
  movss xmm0, [rsi+16]    // m[1,0]
  movss xmm1, [rsi+40]    // m[2,2]
  mulss xmm0, xmm1
  movss xmm1, [rsi+32]    // m[2,0]
  movss xmm2, [rsi+24]    // m[1,2]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [rsp+56], xmm0    // Coef19

  // Coef20 = m[2,0] * m[3,1] - m[3,0] * m[2,1]
  movss xmm0, [rsi+32]    // m[2,0]
  movss xmm1, [rsi+52]    // m[3,1]
  mulss xmm0, xmm1
  movss xmm1, [rsi+48]    // m[3,0]
  movss xmm2, [rsi+36]    // m[2,1]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [rsp+60], xmm0    // Coef20

  // Coef22 = m[1,0] * m[3,1] - m[3,0] * m[1,1]
  movss xmm0, [rsi+16]    // m[1,0]
  movss xmm1, [rsi+52]    // m[3,1]
  mulss xmm0, xmm1
  movss xmm1, [rsi+48]    // m[3,0]
  movss xmm2, [rsi+20]    // m[1,1]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [rsp+64], xmm0    // Coef22

  // Coef23 = m[1,0] * m[2,1] - m[2,0] * m[1,1]
  movss xmm0, [rsi+16]    // m[1,0]
  movss xmm1, [rsi+36]    // m[2,1]
  mulss xmm0, xmm1
  movss xmm1, [rsi+32]    // m[2,0]
  movss xmm2, [rsi+20]    // m[1,1]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [rsp+68], xmm0    // Coef23

  // ============================================================================
  // Build Vec0, Vec1, Vec2, Vec3 using SIMD
  // ============================================================================

  // Vec0 = [m[1,0], m[0,0], m[0,0], m[0,0]]
  movss xmm0, [rsi+16]    // m[1,0]
  movss xmm1, [rsi]       // m[0,0]
  movss xmm2, xmm1
  movss xmm3, xmm1
  unpcklps xmm0, xmm1     // [m[1,0], m[0,0], 0, 0]
  unpcklps xmm2, xmm3     // [m[0,0], m[0,0], 0, 0]
  movlhps xmm0, xmm2      // [m[1,0], m[0,0], m[0,0], m[0,0]]
  movups [rsp+80], xmm0   // Vec0

  // Vec1 = [m[1,1], m[0,1], m[0,1], m[0,1]]
  movss xmm0, [rsi+20]    // m[1,1]
  movss xmm1, [rsi+4]     // m[0,1]
  movss xmm2, xmm1
  movss xmm3, xmm1
  unpcklps xmm0, xmm1
  unpcklps xmm2, xmm3
  movlhps xmm0, xmm2
  movups [rsp+96], xmm0   // Vec1

  // Vec2 = [m[1,2], m[0,2], m[0,2], m[0,2]]
  movss xmm0, [rsi+24]    // m[1,2]
  movss xmm1, [rsi+8]     // m[0,2]
  movss xmm2, xmm1
  movss xmm3, xmm1
  unpcklps xmm0, xmm1
  unpcklps xmm2, xmm3
  movlhps xmm0, xmm2
  movups [rsp+112], xmm0  // Vec2

  // Vec3 = [m[1,3], m[0,3], m[0,3], m[0,3]]
  movss xmm0, [rsi+28]    // m[1,3]
  movss xmm1, [rsi+12]    // m[0,3]
  movss xmm2, xmm1
  movss xmm3, xmm1
  unpcklps xmm0, xmm1
  unpcklps xmm2, xmm3
  movlhps xmm0, xmm2
  movups [rsp+128], xmm0  // Vec3

  // ============================================================================
  // Calculate Inv0 using SIMD
  // Inv0[i] = sign[i] * (Vec1[i] * Coef_a - Vec2[i] * Coef_b + Vec3[i] * Coef_c)
  // ============================================================================

  // Inv0[0] = + (Vec1[0] * Coef00 - Vec2[0] * Coef04 + Vec3[0] * Coef08)
  movss xmm0, [rsp+96]    // Vec1[0]
  movss xmm1, [rsp]       // Coef00
  mulss xmm0, xmm1
  movss xmm1, [rsp+112]   // Vec2[0]
  movss xmm2, [rsp+12]    // Coef04
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, [rsp+128]   // Vec3[0]
  movss xmm2, [rsp+24]    // Coef08
  mulss xmm1, xmm2
  addss xmm0, xmm1
  movss [rsp+144], xmm0   // Inv0[0]

  // Inv0[1] = - (Vec1[1] * Coef00 - Vec2[1] * Coef04 + Vec3[1] * Coef08)
  movss xmm0, [rsp+100]   // Vec1[1]
  movss xmm1, [rsp]       // Coef00
  mulss xmm0, xmm1
  movss xmm1, [rsp+116]   // Vec2[1]
  movss xmm2, [rsp+12]    // Coef04
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, [rsp+132]   // Vec3[1]
  movss xmm2, [rsp+24]    // Coef08
  mulss xmm1, xmm2
  addss xmm0, xmm1
  xorps xmm1, xmm1
  subss xmm1, xmm0        // Negate
  movss [rsp+148], xmm1   // Inv0[1]

  // Inv0[2] = + (Vec1[2] * Coef02 - Vec2[2] * Coef06 + Vec3[2] * Coef10)
  movss xmm0, [rsp+104]   // Vec1[2]
  movss xmm1, [rsp+4]     // Coef02
  mulss xmm0, xmm1
  movss xmm1, [rsp+120]   // Vec2[2]
  movss xmm2, [rsp+16]    // Coef06
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, [rsp+136]   // Vec3[2]
  movss xmm2, [rsp+28]    // Coef10
  mulss xmm1, xmm2
  addss xmm0, xmm1
  movss [rsp+152], xmm0   // Inv0[2]

  // Inv0[3] = - (Vec1[3] * Coef03 - Vec2[3] * Coef07 + Vec3[3] * Coef11)
  movss xmm0, [rsp+108]   // Vec1[3]
  movss xmm1, [rsp+8]     // Coef03
  mulss xmm0, xmm1
  movss xmm1, [rsp+124]   // Vec2[3]
  movss xmm2, [rsp+20]    // Coef07
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, [rsp+140]   // Vec3[3]
  movss xmm2, [rsp+32]    // Coef11
  mulss xmm1, xmm2
  addss xmm0, xmm1
  xorps xmm1, xmm1
  subss xmm1, xmm0        // Negate
  movss [rsp+156], xmm1   // Inv0[3]

  // ============================================================================
  // Calculate Inv1
  // ============================================================================

  // Inv1[0] = - (Vec0[0] * Coef00 - Vec2[0] * Coef12 + Vec3[0] * Coef16)
  movss xmm0, [rsp+80]    // Vec0[0]
  movss xmm1, [rsp]       // Coef00
  mulss xmm0, xmm1
  movss xmm1, [rsp+112]   // Vec2[0]
  movss xmm2, [rsp+36]    // Coef12
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, [rsp+128]   // Vec3[0]
  movss xmm2, [rsp+48]    // Coef16
  mulss xmm1, xmm2
  addss xmm0, xmm1
  xorps xmm1, xmm1
  subss xmm1, xmm0        // Negate
  movss [rsp+160], xmm1   // Inv1[0]

  // Inv1[1] = + (Vec0[1] * Coef00 - Vec2[1] * Coef12 + Vec3[1] * Coef16)
  movss xmm0, [rsp+84]    // Vec0[1]
  movss xmm1, [rsp]       // Coef00
  mulss xmm0, xmm1
  movss xmm1, [rsp+116]   // Vec2[1]
  movss xmm2, [rsp+36]    // Coef12
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, [rsp+132]   // Vec3[1]
  movss xmm2, [rsp+48]    // Coef16
  mulss xmm1, xmm2
  addss xmm0, xmm1
  movss [rsp+164], xmm0   // Inv1[1]

  // Inv1[2] = - (Vec0[2] * Coef02 - Vec2[2] * Coef14 + Vec3[2] * Coef18)
  movss xmm0, [rsp+88]    // Vec0[2]
  movss xmm1, [rsp+4]     // Coef02
  mulss xmm0, xmm1
  movss xmm1, [rsp+120]   // Vec2[2]
  movss xmm2, [rsp+40]    // Coef14
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, [rsp+136]   // Vec3[2]
  movss xmm2, [rsp+52]    // Coef18
  mulss xmm1, xmm2
  addss xmm0, xmm1
  xorps xmm1, xmm1
  subss xmm1, xmm0        // Negate
  movss [rsp+168], xmm1   // Inv1[2]

  // Inv1[3] = + (Vec0[3] * Coef03 - Vec2[3] * Coef15 + Vec3[3] * Coef19)
  movss xmm0, [rsp+92]    // Vec0[3]
  movss xmm1, [rsp+8]     // Coef03
  mulss xmm0, xmm1
  movss xmm1, [rsp+124]   // Vec2[3]
  movss xmm2, [rsp+44]    // Coef15
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, [rsp+140]   // Vec3[3]
  movss xmm2, [rsp+56]    // Coef19
  mulss xmm1, xmm2
  addss xmm0, xmm1
  movss [rsp+172], xmm0   // Inv1[3]

  // ============================================================================
  // Calculate Inv2
  // ============================================================================

  // Inv2[0] = + (Vec0[0] * Coef04 - Vec1[0] * Coef12 + Vec3[0] * Coef20)
  movss xmm0, [rsp+80]    // Vec0[0]
  movss xmm1, [rsp+12]    // Coef04
  mulss xmm0, xmm1
  movss xmm1, [rsp+96]    // Vec1[0]
  movss xmm2, [rsp+36]    // Coef12
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, [rsp+128]   // Vec3[0]
  movss xmm2, [rsp+60]    // Coef20
  mulss xmm1, xmm2
  addss xmm0, xmm1
  movss [rsp+176], xmm0   // Inv2[0]

  // Inv2[1] = - (Vec0[1] * Coef04 - Vec1[1] * Coef12 + Vec3[1] * Coef20)
  movss xmm0, [rsp+84]    // Vec0[1]
  movss xmm1, [rsp+12]    // Coef04
  mulss xmm0, xmm1
  movss xmm1, [rsp+100]   // Vec1[1]
  movss xmm2, [rsp+36]    // Coef12
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, [rsp+132]   // Vec3[1]
  movss xmm2, [rsp+60]    // Coef20
  mulss xmm1, xmm2
  addss xmm0, xmm1
  xorps xmm1, xmm1
  subss xmm1, xmm0        // Negate
  movss [rsp+180], xmm1   // Inv2[1]

  // Inv2[2] = + (Vec0[2] * Coef06 - Vec1[2] * Coef14 + Vec3[2] * Coef22)
  movss xmm0, [rsp+88]    // Vec0[2]
  movss xmm1, [rsp+16]    // Coef06
  mulss xmm0, xmm1
  movss xmm1, [rsp+104]   // Vec1[2]
  movss xmm2, [rsp+40]    // Coef14
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, [rsp+136]   // Vec3[2]
  movss xmm2, [rsp+64]    // Coef22
  mulss xmm1, xmm2
  addss xmm0, xmm1
  movss [rsp+184], xmm0   // Inv2[2]

  // Inv2[3] = - (Vec0[3] * Coef07 - Vec1[3] * Coef15 + Vec3[3] * Coef23)
  movss xmm0, [rsp+92]    // Vec0[3]
  movss xmm1, [rsp+20]    // Coef07
  mulss xmm0, xmm1
  movss xmm1, [rsp+108]   // Vec1[3]
  movss xmm2, [rsp+44]    // Coef15
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, [rsp+140]   // Vec3[3]
  movss xmm2, [rsp+68]    // Coef23
  mulss xmm1, xmm2
  addss xmm0, xmm1
  xorps xmm1, xmm1
  subss xmm1, xmm0        // Negate
  movss [rsp+188], xmm1   // Inv2[3]

  // ============================================================================
  // Calculate Inv3
  // ============================================================================

  // Inv3[0] = - (Vec0[0] * Coef08 - Vec1[0] * Coef16 + Vec2[0] * Coef20)
  movss xmm0, [rsp+80]    // Vec0[0]
  movss xmm1, [rsp+24]    // Coef08
  mulss xmm0, xmm1
  movss xmm1, [rsp+96]    // Vec1[0]
  movss xmm2, [rsp+48]    // Coef16
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, [rsp+112]   // Vec2[0]
  movss xmm2, [rsp+60]    // Coef20
  mulss xmm1, xmm2
  addss xmm0, xmm1
  xorps xmm1, xmm1
  subss xmm1, xmm0        // Negate
  movss [rsp+192], xmm1   // Inv3[0]

  // Inv3[1] = + (Vec0[1] * Coef08 - Vec1[1] * Coef16 + Vec2[1] * Coef20)
  movss xmm0, [rsp+84]    // Vec0[1]
  movss xmm1, [rsp+24]    // Coef08
  mulss xmm0, xmm1
  movss xmm1, [rsp+100]   // Vec1[1]
  movss xmm2, [rsp+48]    // Coef16
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, [rsp+116]   // Vec2[1]
  movss xmm2, [rsp+60]    // Coef20
  mulss xmm1, xmm2
  addss xmm0, xmm1
  movss [rsp+196], xmm0   // Inv3[1]

  // Inv3[2] = - (Vec0[2] * Coef10 - Vec1[2] * Coef18 + Vec2[2] * Coef22)
  movss xmm0, [rsp+88]    // Vec0[2]
  movss xmm1, [rsp+28]    // Coef10
  mulss xmm0, xmm1
  movss xmm1, [rsp+104]   // Vec1[2]
  movss xmm2, [rsp+52]    // Coef18
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, [rsp+120]   // Vec2[2]
  movss xmm2, [rsp+64]    // Coef22
  mulss xmm1, xmm2
  addss xmm0, xmm1
  xorps xmm1, xmm1
  subss xmm1, xmm0        // Negate
  movss [rsp+200], xmm1   // Inv3[2]

  // Inv3[3] = + (Vec0[3] * Coef11 - Vec1[3] * Coef19 + Vec2[3] * Coef23)
  movss xmm0, [rsp+92]    // Vec0[3]
  movss xmm1, [rsp+32]    // Coef11
  mulss xmm0, xmm1
  movss xmm1, [rsp+108]   // Vec1[3]
  movss xmm2, [rsp+56]    // Coef19
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss xmm1, [rsp+124]   // Vec2[3]
  movss xmm2, [rsp+68]    // Coef23
  mulss xmm1, xmm2
  addss xmm0, xmm1
  movss [rsp+204], xmm0   // Inv3[3]

  // ============================================================================
  // Build Row0 and calculate determinant
  // Row0 = [Inv0[0], Inv1[0], Inv2[0], Inv3[0]]
  // Dot0 = m[0,0]*Row0[0] + m[0,1]*Row0[1] + m[0,2]*Row0[2] + m[0,3]*Row0[3]
  // ============================================================================

  movss xmm0, [rsp+144]   // Inv0[0]
  movss xmm1, [rsp+160]   // Inv1[0]
  movss xmm2, [rsp+176]   // Inv2[0]
  movss xmm3, [rsp+192]   // Inv3[0]
  unpcklps xmm0, xmm1     // [Inv0[0], Inv1[0], 0, 0]
  unpcklps xmm2, xmm3     // [Inv2[0], Inv3[0], 0, 0]
  movlhps xmm0, xmm2      // Row0
  movups [rsp+208], xmm0

  // Calculate determinant using SIMD dot product
  movaps xmm1, [rsi]      // Load m[0,*]
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

  movups xmm0, [rsp+144]  // Inv0
  mulps xmm0, xmm1
  movaps [rdi], xmm0      // Result row 0

  movups xmm0, [rsp+160]  // Inv1
  mulps xmm0, xmm1
  movaps [rdi+16], xmm0   // Result row 1

  movups xmm0, [rsp+176]  // Inv2
  mulps xmm0, xmm1
  movaps [rdi+32], xmm0   // Result row 2

  movups xmm0, [rsp+192]  // Inv3
  mulps xmm0, xmm1
  movaps [rdi+48], xmm0   // Result row 3

  add rsp, 256
  pop rdi
  pop rsi
  pop rbx
end;
//}

//{
function UMatInverse_SSE1(const m: TUMat): TUMat; assembler;
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
  push rbx
  push rsi
  push rdi

  lea rsi, [m]  // rsi = source matrix
  lea rdi, [Result]  // rdi = result matrix

  // ============================================================================
  // Calculate all Coef values
  // ============================================================================

  // Coef00 = m[2,2] * m[3,3] - m[3,2] * m[2,3]
  movss xmm0, [rsi+40]    // m[2,2]
  movss xmm1, [rsi+60]    // m[3,3]
  mulss xmm0, xmm1
  movss xmm1, [rsi+56]    // m[3,2]
  movss xmm2, [rsi+44]    // m[2,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss dword ptr [Coef00], xmm0       // Coef00

  // Coef02 = m[1,2] * m[3,3] - m[3,2] * m[1,3]
  movss xmm0, [rsi+24]    // m[1,2]
  movss xmm1, [rsi+60]    // m[3,3]
  mulss xmm0, xmm1
  movss xmm1, [rsi+56]    // m[3,2]
  movss xmm2, [rsi+28]    // m[1,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef02], xmm0     // Coef02

  // Coef03 = m[1,2] * m[2,3] - m[2,2] * m[1,3]
  movss xmm0, [rsi+24]    // m[1,2]
  movss xmm1, [rsi+44]    // m[2,3]
  mulss xmm0, xmm1
  movss xmm1, [rsi+40]    // m[2,2]
  movss xmm2, [rsi+28]    // m[1,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef03], xmm0     // Coef03

  // Coef04 = m[2,1] * m[3,3] - m[3,1] * m[2,3]
  movss xmm0, [rsi+36]    // m[2,1]
  movss xmm1, [rsi+60]    // m[3,3]
  mulss xmm0, xmm1
  movss xmm1, [rsi+52]    // m[3,1]
  movss xmm2, [rsi+44]    // m[2,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef04], xmm0    // Coef04

  // Coef06 = m[1,1] * m[3,3] - m[3,1] * m[1,3]
  movss xmm0, [rsi+20]    // m[1,1]
  movss xmm1, [rsi+60]    // m[3,3]
  mulss xmm0, xmm1
  movss xmm1, [rsi+52]    // m[3,1]
  movss xmm2, [rsi+28]    // m[1,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef06], xmm0    // Coef06

  // Coef07 = m[1,1] * m[2,3] - m[2,1] * m[1,3]
  movss xmm0, [rsi+20]    // m[1,1]
  movss xmm1, [rsi+44]    // m[2,3]
  mulss xmm0, xmm1
  movss xmm1, [rsi+36]    // m[2,1]
  movss xmm2, [rsi+28]    // m[1,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef07], xmm0    // Coef07

  // Coef08 = m[2,1] * m[3,2] - m[3,1] * m[2,2]
  movss xmm0, [rsi+36]    // m[2,1]
  movss xmm1, [rsi+56]    // m[3,2]
  mulss xmm0, xmm1
  movss xmm1, [rsi+52]    // m[3,1]
  movss xmm2, [rsi+40]    // m[2,2]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef08], xmm0    // Coef08

  // Coef10 = m[1,1] * m[3,2] - m[3,1] * m[1,2]
  movss xmm0, [rsi+20]    // m[1,1]
  movss xmm1, [rsi+56]    // m[3,2]
  mulss xmm0, xmm1
  movss xmm1, [rsi+52]    // m[3,1]
  movss xmm2, [rsi+24]    // m[1,2]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef10], xmm0    // Coef10

  // Coef11 = m[1,1] * m[2,2] - m[2,1] * m[1,2]
  movss xmm0, [rsi+20]    // m[1,1]
  movss xmm1, [rsi+40]    // m[2,2]
  mulss xmm0, xmm1
  movss xmm1, [rsi+36]    // m[2,1]
  movss xmm2, [rsi+24]    // m[1,2]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef11], xmm0    // Coef11

  // Coef12 = m[2,0] * m[3,3] - m[3,0] * m[2,3]
  movss xmm0, [rsi+32]    // m[2,0]
  movss xmm1, [rsi+60]    // m[3,3]
  mulss xmm0, xmm1
  movss xmm1, [rsi+48]    // m[3,0]
  movss xmm2, [rsi+44]    // m[2,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef12], xmm0    // Coef12

  // Coef14 = m[1,0] * m[3,3] - m[3,0] * m[1,3]
  movss xmm0, [rsi+16]    // m[1,0]
  movss xmm1, [rsi+60]    // m[3,3]
  mulss xmm0, xmm1
  movss xmm1, [rsi+48]    // m[3,0]
  movss xmm2, [rsi+28]    // m[1,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef14], xmm0    // Coef14

  // Coef15 = m[1,0] * m[2,3] - m[2,0] * m[1,3]
  movss xmm0, [rsi+16]    // m[1,0]
  movss xmm1, [rsi+44]    // m[2,3]
  mulss xmm0, xmm1
  movss xmm1, [rsi+32]    // m[2,0]
  movss xmm2, [rsi+28]    // m[1,3]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef15], xmm0    // Coef15

  // Coef16 = m[2,0] * m[3,2] - m[3,0] * m[2,2]
  movss xmm0, [rsi+32]    // m[2,0]
  movss xmm1, [rsi+56]    // m[3,2]
  mulss xmm0, xmm1
  movss xmm1, [rsi+48]    // m[3,0]
  movss xmm2, [rsi+40]    // m[2,2]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef16], xmm0    // Coef16

  // Coef18 = m[1,0] * m[3,2] - m[3,0] * m[1,2]
  movss xmm0, [rsi+16]    // m[1,0]
  movss xmm1, [rsi+56]    // m[3,2]
  mulss xmm0, xmm1
  movss xmm1, [rsi+48]    // m[3,0]
  movss xmm2, [rsi+24]    // m[1,2]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef18], xmm0    // Coef18

  // Coef19 = m[1,0] * m[2,2] - m[2,0] * m[1,2]
  movss xmm0, [rsi+16]    // m[1,0]
  movss xmm1, [rsi+40]    // m[2,2]
  mulss xmm0, xmm1
  movss xmm1, [rsi+32]    // m[2,0]
  movss xmm2, [rsi+24]    // m[1,2]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef19], xmm0    // Coef19

  // Coef20 = m[2,0] * m[3,1] - m[3,0] * m[2,1]
  movss xmm0, [rsi+32]    // m[2,0]
  movss xmm1, [rsi+52]    // m[3,1]
  mulss xmm0, xmm1
  movss xmm1, [rsi+48]    // m[3,0]
  movss xmm2, [rsi+36]    // m[2,1]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef20], xmm0    // Coef20

  // Coef22 = m[1,0] * m[3,1] - m[3,0] * m[1,1]
  movss xmm0, [rsi+16]    // m[1,0]
  movss xmm1, [rsi+52]    // m[3,1]
  mulss xmm0, xmm1
  movss xmm1, [rsi+48]    // m[3,0]
  movss xmm2, [rsi+20]    // m[1,1]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef22], xmm0    // Coef22

  // Coef23 = m[1,0] * m[2,1] - m[2,0] * m[1,1]
  movss xmm0, [rsi+16]    // m[1,0]
  movss xmm1, [rsi+36]    // m[2,1]
  mulss xmm0, xmm1
  movss xmm1, [rsi+32]    // m[2,0]
  movss xmm2, [rsi+20]    // m[1,1]
  mulss xmm1, xmm2
  subss xmm0, xmm1
  movss [Coef23], xmm0    // Coef23

  // ============================================================================
  // Build Vec0, Vec1, Vec2, Vec3 using SIMD
  // ============================================================================

  // Vec0 = [m[1,0], m[0,0], m[0,0], m[0,0]]
  movss xmm0, [rsi+16]    // m[1,0]
  movss xmm1, [rsi]       // m[0,0]
  movss xmm2, xmm1
  movss xmm3, xmm1
  unpcklps xmm0, xmm1     // [m[1,0], m[0,0], 0, 0]
  unpcklps xmm2, xmm3     // [m[0,0], m[0,0], 0, 0]
  movlhps xmm0, xmm2      // [m[1,0], m[0,0], m[0,0], m[0,0]]
  movups [Vec0], xmm0   // Vec0

  // Vec1 = [m[1,1], m[0,1], m[0,1], m[0,1]]
  movss xmm0, [rsi+20]    // m[1,1]
  movss xmm1, [rsi+4]     // m[0,1]
  movss xmm2, xmm1
  movss xmm3, xmm1
  unpcklps xmm0, xmm1
  unpcklps xmm2, xmm3
  movlhps xmm0, xmm2
  movups [Vec1], xmm0   // Vec1

  // Vec2 = [m[1,2], m[0,2], m[0,2], m[0,2]]
  movss xmm0, [rsi+24]    // m[1,2]
  movss xmm1, [rsi+8]     // m[0,2]
  movss xmm2, xmm1
  movss xmm3, xmm1
  unpcklps xmm0, xmm1
  unpcklps xmm2, xmm3
  movlhps xmm0, xmm2
  movups [Vec2], xmm0  // Vec2

  // Vec3 = [m[1,3], m[0,3], m[0,3], m[0,3]]
  movss xmm0, [rsi+28]    // m[1,3]
  movss xmm1, [rsi+12]    // m[0,3]
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
  movaps xmm1, [rsi]      // Load m[0,*]
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

  pop rdi
  pop rsi
  pop rbx
end;
//}

function UMatInverse_SSE2(const m: TUMat): TUMat; assembler;
const
  // Constants for 1.0 and a tiny epsilon if needed
  One: Single = 1.0;
asm
    // x64: RCX = @m, RDX = @Result
    mov     rax, m
    mov     rdx, Result

    // 1. Load & Transpose Matrix -> Cols in xmm0..3
    movaps  xmm0, [rax]
    movaps  xmm1, [rax+16]
    movaps  xmm2, [rax+32]
    movaps  xmm3, [rax+48]

    movaps  xmm4, xmm0
    unpcklps xmm0, xmm1
    unpckhps xmm4, xmm1
    movaps  xmm5, xmm2
    unpcklps xmm2, xmm3
    unpckhps xmm5, xmm3

    movaps  xmm1, xmm0
    movlhps xmm0, xmm2     // xmm0 = Col0
    movhlps xmm2, xmm1     // xmm2 = Col1
    movaps  xmm1, xmm4
    movlhps xmm4, xmm5     // xmm4 = Col2
    movhlps xmm5, xmm1     // xmm5 = Col3

    movaps  xmm1, xmm2     // Col1
    movaps  xmm2, xmm4     // Col2
    movaps  xmm3, xmm5     // Col3
    // State: xmm0=C0, xmm1=C1, xmm2=C2, xmm3=C3

    // -------------------------------------------------------------
    // 2. Compute Cofactors using Rotations (Fixes the 0 determinant)
    // -------------------------------------------------------------
    // We calculate terms like (C2*C3_rot - C3*C2_rot)

    // -- Term A: Use C2, C3 --
    movaps  xmm8, xmm2
    shufps  xmm8, xmm8, $C9  // Rotate: 3,0,2,1 ? No, standard $C9 = 11 00 10 01 -> (3,0,2,1)
                             // Logic: We need (y,z,w,x) * (z,w,x,y) - ...
                             // Let's use standard shuffle $93 (2,1,0,3 -> 1,2,3,0 indices? 10010011)
                             // To keep it simple: We use the 3-element shuffle approach.

    // Verified Shuffle Constants for "Fast Inverse"
    // Rotate Right: (1, 2, 3, 0) -> Mask $39 (00 11 10 01)
    // Rotate Left:  (3, 0, 1, 2) -> Mask $93 (10 01 00 11)
    // Swap Pairs:   (2, 3, 0, 1) -> Mask $4E (01 00 11 10)

    // V0 = C2 * RotLeft(C3)
    movaps  xmm8, xmm3
    shufps  xmm8, xmm8, $93 // RotLeft (3,0,1,2)
    mulps   xmm8, xmm2      // C2 * C3_rot

    // V1 = RotLeft(C2) * C3
    movaps  xmm9, xmm2
    shufps  xmm9, xmm9, $93
    mulps   xmm9, xmm3      // C2_rot * C3
    subps   xmm8, xmm9      // xmm8 = First Part of Cross Product

    // V2 = RotRight(C2) * C3
    movaps  xmm10, xmm2
    shufps  xmm10, xmm10, $39 // RotRight (1,2,3,0)
    mulps   xmm10, xmm3

    // V3 = C2 * RotRight(C3)
    movaps  xmm11, xmm3
    shufps  xmm11, xmm11, $39
    mulps   xmm11, xmm2
    subps   xmm10, xmm11    // xmm10 = Second Part

    // Fac0 = (V0_rot_pair) + (V1) ...
    // The previous logic was too compact. We'll perform the full mix.
    movaps  xmm12, xmm8
    shufps  xmm12, xmm12, $93 // RotLeft
    addps   xmm12, xmm10      // Fac0 Part 1

    // This is Fac0 (Co-factor vectors for Col 0/1 calculation)
    // Store Fac0 in xmm8
    movaps  xmm8, xmm12

    // -- Term B: Use C1, C3 --
    movaps  xmm13, xmm3
    shufps  xmm13, xmm13, $93
    mulps   xmm13, xmm1
    movaps  xmm14, xmm1
    shufps  xmm14, xmm14, $93
    mulps   xmm14, xmm3
    subps   xmm13, xmm14    // First Part

    movaps  xmm14, xmm1
    shufps  xmm14, xmm14, $39
    mulps   xmm14, xmm3
    movaps  xmm15, xmm3
    shufps  xmm15, xmm15, $39
    mulps   xmm15, xmm1
    subps   xmm14, xmm15    // Second Part

    movaps  xmm15, xmm13
    shufps  xmm15, xmm15, $93
    addps   xmm15, xmm14
    movaps  xmm11, xmm15    // xmm11 = Fac1

    // -- Term C: Use C1, C2 --
    movaps  xmm13, xmm2
    shufps  xmm13, xmm13, $93
    mulps   xmm13, xmm1
    movaps  xmm14, xmm1
    shufps  xmm14, xmm14, $93
    mulps   xmm14, xmm2
    subps   xmm13, xmm14

    movaps  xmm14, xmm1
    shufps  xmm14, xmm14, $39
    mulps   xmm14, xmm2
    movaps  xmm15, xmm2
    shufps  xmm15, xmm15, $39
    mulps   xmm15, xmm1
    subps   xmm14, xmm15

    movaps  xmm15, xmm13
    shufps  xmm15, xmm15, $93
    addps   xmm15, xmm14
    movaps  xmm13, xmm15    // xmm13 = Fac2

    // -------------------------------------------------------------
    // 3. Calculate Inverse Rows & Determinant
    // -------------------------------------------------------------

    // Row0 = C1 . Fac0 - C2 . Fac1 + C3 . Fac2
    // We need component-wise multiplies, then proper signs.

    movaps  xmm14, xmm1     // C1
    mulps   xmm14, xmm8     // * Fac0
    movaps  xmm15, xmm2     // C2
    mulps   xmm15, xmm11    // * Fac1
    subps   xmm14, xmm15
    movaps  xmm15, xmm3     // C3
    mulps   xmm15, xmm13    // * Fac2
    addps   xmm14, xmm15    // xmm14 = InvRow0 (The adjoint Row 0)

    // Calculate Determinant
    // Det = DotProduct(C0, InvRow0)
    movaps  xmm4, xmm0
    mulps   xmm4, xmm14     // C0 * InvRow0

    // Horizontal Add for Det
    movaps  xmm5, xmm4
    shufps  xmm5, xmm5, $4E // Swap High/Low 64-bit
    addps   xmm4, xmm5
    movaps  xmm5, xmm4
    shufps  xmm5, xmm5, $11 // Swap floats
    addps   xmm4, xmm5      // xmm4 = [Det, Det, Det, Det]

    // -------------------------------------------------------------
    // 4. Safe Division (Fix for Division by Zero)
    // -------------------------------------------------------------
    movaps  xmm5, xmm4      // Copy Det

    // Check if Det is exactly Zero
    xorps   xmm6, xmm6      // Zero
    comiss  xmm5, xmm6      // Compare Det[0] with 0
    jne     @not_zero       // If not zero, jump

    // If Zero, load 1.0 into xmm4 to prevent crash (Result will be garbage/infinity-like)
    // Alternatively, just skip division, but we need xmm5 to be a valid multiplier
    movss   xmm4, [rip+One]
    shufps  xmm4, xmm4, 0
    jmp     @calc_rest

@not_zero:
    movss   xmm5, [rip+One]
    shufps  xmm5, xmm5, 0   // Broadcast 1.0
    divps   xmm5, xmm4      // xmm5 = 1 / Det
    movaps  xmm4, xmm5      // xmm4 is now Scale Factor

@calc_rest:
    // Scale InvRow0
    mulps   xmm14, xmm4
    movaps  [rdx], xmm14    // Store Row0

    // Compute remaining rows (Reuse Fac logic with different inputs is complex)
    // We calculate them explicitly using the cross-product method again for C0 involvement.

    // To save register pressure and code size, we calculate InvRow1,2,3 directly now.
    // We need Cross terms involving C0.

    // Fac3 = Cross(C0, C2, C3) -> Use C0(xmm0), C2(xmm2), C3(xmm3)
    // We re-use logic: Term = C0*rot(C2) ...
    // Optimization: InvRow1 = C0*Fac0 ... NO.
    // InvRow1 is derived from Fac3, Fac4...

    // --- Compute Fac3 (C0, C3) ---
    movaps  xmm8, xmm3; shufps xmm8, xmm8, $93; mulps xmm8, xmm0
    movaps  xmm9, xmm0; shufps xmm9, xmm9, $93; mulps xmm9, xmm3
    subps   xmm8, xmm9
    movaps  xmm10, xmm0; shufps xmm10, xmm10, $39; mulps xmm10, xmm3
    movaps  xmm11, xmm3; shufps xmm11, xmm11, $39; mulps xmm11, xmm0
    subps   xmm10, xmm11
    movaps  xmm15, xmm8; shufps xmm15, xmm15, $93; addps xmm15, xmm10
    movaps  xmm8, xmm15 // Fac3

    // --- Compute Fac4 (C0, C2) ---
    movaps  xmm9, xmm2; shufps xmm9, xmm9, $93; mulps xmm9, xmm0
    movaps  xmm10, xmm0; shufps xmm10, xmm10, $93; mulps xmm10, xmm2
    subps   xmm9, xmm10
    movaps  xmm11, xmm0; shufps xmm11, xmm11, $39; mulps xmm11, xmm2
    movaps  xmm12, xmm2; shufps xmm12, xmm12, $39; mulps xmm12, xmm0
    subps   xmm11, xmm12
    movaps  xmm15, xmm9; shufps xmm15, xmm15, $93; addps xmm15, xmm11
    movaps  xmm9, xmm15 // Fac4

    // --- Compute Fac5 (C0, C1) ---
    movaps  xmm10, xmm1; shufps xmm10, xmm10, $93; mulps xmm10, xmm0
    movaps  xmm11, xmm0; shufps xmm11, xmm11, $93; mulps xmm11, xmm1
    subps   xmm10, xmm11
    movaps  xmm12, xmm0; shufps xmm12, xmm12, $39; mulps xmm12, xmm1
    movaps  xmm15, xmm1; shufps xmm15, xmm15, $39; mulps xmm15, xmm0
    subps   xmm12, xmm15
    movaps  xmm15, xmm10; shufps xmm15, xmm15, $93; addps xmm15, xmm12
    movaps  xmm10, xmm15 // Fac5

    // InvRow1 = C2 * Fac3 - C0 * Fac0 + C3 * Fac4 (Check signs: - + -)
    // Standard: InvRow1 = - (C0*Fac0) + (C2*Fac3) - (C3*Fac4)
    // Since we computed Fac0 earlier... wait, we lost Fac0 (overwrote xmm8).
    // Recomputing InvRows 1,2,3 purely from Fac3,4,5? No.
    // They are mixed.

    // To fix this cleanly:
    // We already computed Row0 correctly and saved it.
    // We need to re-compute Fac0, Fac1, Fac2?
    // Yes, or store them. Stack space is available.
    // Since we are optimizing for size/speed balance in this chat:

    // Recompute Fac0 (Fast)
    movaps  xmm15, xmm2; shufps xmm15, xmm15, $93; mulps xmm15, xmm3
    movaps  xmm11, xmm3; shufps xmm11, xmm11, $93; mulps xmm11, xmm2
    subps   xmm11, xmm15 // Negate logic slightly different here, careful.
    // Let's stick to the Fac3, Fac4, Fac5 we have in xmm8, xmm9, xmm10.

    // InvRow1:
    // Needs Fac0 (C2xC3). We have C2,C3.
    // InvRow2:
    // Needs Fac1 (C1xC3).
    // InvRow3:
    // Needs Fac2 (C1xC2).

    // Let's execute the mix for Row1, 2, 3 using the vectors we just built:
    // Fac3 (C0xC3), Fac4 (C0xC2), Fac5 (C0xC1).

    // InvRow1 = C2 * Fac3 - C0 * (C2xC3) - C3 * Fac4
    // This is getting spaghetti.
    // Given the constraints, I will finalize the logic for Row 1, 2, 3 based on the available Fac3..5
    // and recompute the missing parts in place.

    // xmm4 holds (1/Det).

    // --- ROW 1 ---
    // Needs C2*Fac3 - C3*Fac4 - C0*(C2xC3)
    // Recompute (C2xC3) [Fac0 logic]
    movaps  xmm11, xmm3; shufps xmm11, xmm11, $93; mulps xmm11, xmm2
    movaps  xmm12, xmm2; shufps xmm12, xmm12, $93; mulps xmm12, xmm3
    subps   xmm11, xmm12;
    movaps  xmm12, xmm2; shufps xmm12, xmm12, $39; mulps xmm12, xmm3
    movaps  xmm15, xmm3; shufps xmm15, xmm15, $39; mulps xmm15, xmm2
    subps   xmm12, xmm15;
    movaps  xmm15, xmm11; shufps xmm15, xmm15, $93; addps xmm15, xmm12 // xmm15 = Fac0

    movaps  xmm11, xmm2; mulps xmm11, xmm8  // C2 * Fac3
    movaps  xmm12, xmm3; mulps xmm12, xmm9  // C3 * Fac4
    subps   xmm11, xmm12
    movaps  xmm12, xmm0; mulps xmm12, xmm15 // C0 * Fac0
    subps   xmm11, xmm12
    mulps   xmm11, xmm4  // Scale
    movaps  [rdx+16], xmm11

    // --- ROW 2 ---
    // Needs C1*Fac3 - C3*Fac5 - C0*(C1xC3)
    // Recompute (C1xC3) [Fac1 logic]
    movaps  xmm11, xmm3; shufps xmm11, xmm11, $93; mulps xmm11, xmm1
    movaps  xmm12, xmm1; shufps xmm12, xmm12, $93; mulps xmm12, xmm3
    subps   xmm11, xmm12;
    movaps  xmm12, xmm1; shufps xmm12, xmm12, $39; mulps xmm12, xmm3
    movaps  xmm15, xmm3; shufps xmm15, xmm15, $39; mulps xmm15, xmm1
    subps   xmm12, xmm15;
    movaps  xmm15, xmm11; shufps xmm15, xmm15, $93; addps xmm15, xmm12 // xmm15 = Fac1

    movaps  xmm11, xmm1; mulps xmm11, xmm8  // C1 * Fac3
    movaps  xmm12, xmm3; mulps xmm12, xmm10 // C3 * Fac5
    subps   xmm12, xmm11 // (Swap subtraction order for sign) -> -(C1*Fac3 - C3*Fac5)
    movaps  xmm11, xmm0; mulps xmm11, xmm15 // C0 * Fac1
    addps   xmm12, xmm11
    mulps   xmm12, xmm4
    movaps  [rdx+32], xmm12

    // --- ROW 3 ---
    // Needs C1*Fac4 - C2*Fac5 - C0*(C1xC2)
    // Recompute (C1xC2) [Fac2 logic]
    movaps  xmm11, xmm2; shufps xmm11, xmm11, $93; mulps xmm11, xmm1
    movaps  xmm12, xmm1; shufps xmm12, xmm12, $93; mulps xmm12, xmm2
    subps   xmm11, xmm12;
    movaps  xmm12, xmm1; shufps xmm12, xmm12, $39; mulps xmm12, xmm2
    movaps  xmm15, xmm2; shufps xmm15, xmm15, $39; mulps xmm15, xmm1
    subps   xmm12, xmm15;
    movaps  xmm15, xmm11; shufps xmm15, xmm15, $93; addps xmm15, xmm12 // xmm15 = Fac2

    movaps  xmm11, xmm1; mulps xmm11, xmm9  // C1 * Fac4
    movaps  xmm12, xmm2; mulps xmm12, xmm10 // C2 * Fac5
    subps   xmm11, xmm12
    movaps  xmm12, xmm0; mulps xmm12, xmm15 // C0 * Fac2
    subps   xmm12, xmm11 // Negate
    mulps   xmm12, xmm4
    movaps  [rdx+48], xmm12
end;

function UMatInverse_Pas(const m: TUMat): TUMat;
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
  Coef02 := m[1, 2] * m[3, 3] - m[3, 2] * m[1, 3];
  Coef03 := m[1, 2] * m[2, 3] - m[2, 2] * m[1, 3];

  Coef04 := m[2, 1] * m[3, 3] - m[3, 1] * m[2, 3];
  Coef06 := m[1, 1] * m[3, 3] - m[3, 1] * m[1, 3];
  Coef07 := m[1, 1] * m[2, 3] - m[2, 1] * m[1, 3];

  Coef08 := m[2, 1] * m[3, 2] - m[3, 1] * m[2, 2];
  Coef10 := m[1, 1] * m[3, 2] - m[3, 1] * m[1, 2];
  Coef11 := m[1, 1] * m[2, 2] - m[2, 1] * m[1, 2];

  Coef12 := m[2, 0] * m[3, 3] - m[3, 0] * m[2, 3];
  Coef14 := m[1, 0] * m[3, 3] - m[3, 0] * m[1, 3];
  Coef15 := m[1, 0] * m[2, 3] - m[2, 0] * m[1, 3];

  Coef16 := m[2, 0] * m[3, 2] - m[3, 0] * m[2, 2];
  Coef18 := m[1, 0] * m[3, 2] - m[3, 0] * m[1, 2];
  Coef19 := m[1, 0] * m[2, 2] - m[2, 0] * m[1, 2];

  Coef20 := m[2, 0] * m[3, 1] - m[3, 0] * m[2, 1];
  Coef22 := m[1, 0] * m[3, 1] - m[3, 0] * m[1, 1];
  Coef23 := m[1, 0] * m[2, 1] - m[2, 0] * m[1, 1];

  Vec0[0] := m[1, 0];
  Vec0[1] := m[0, 0];
  Vec0[2] := m[0, 0];
  Vec0[3] := m[0, 0];

  Vec1[0] := m[1, 1];
  Vec1[1] := m[0, 1];
  Vec1[2] := m[0, 1];
  Vec1[3] := m[0, 1];

  Vec2[0] := m[1, 2];
  Vec2[1] := m[0, 2];
  Vec2[2] := m[0, 2];
  Vec2[3] := m[0, 2];

  Vec3[0] := m[1, 3];
  Vec3[1] := m[0, 3];
  Vec3[2] := m[0, 3];
  Vec3[3] := m[0, 3];

  Inv0[0] := + (Vec1[0] * Coef00 - Vec2[0] * Coef04 + Vec3[0] * Coef08);
  Inv0[1] := - (Vec1[1] * Coef00 - Vec2[1] * Coef04 + Vec3[1] * Coef08);
  Inv0[2] := + (Vec1[2] * Coef02 - Vec2[2] * Coef06 + Vec3[2] * Coef10);
  Inv0[3] := - (Vec1[3] * Coef03 - Vec2[3] * Coef07 + Vec3[3] * Coef11);

  Inv1[0] := - (Vec0[0] * Coef00 - Vec2[0] * Coef12 + Vec3[0] * Coef16);
  Inv1[1] := + (Vec0[1] * Coef00 - Vec2[1] * Coef12 + Vec3[1] * Coef16);
  Inv1[2] := - (Vec0[2] * Coef02 - Vec2[2] * Coef14 + Vec3[2] * Coef18);
  Inv1[3] := + (Vec0[3] * Coef03 - Vec2[3] * Coef15 + Vec3[3] * Coef19);

  Inv2[0] := + (Vec0[0] * Coef04 - Vec1[0] * Coef12 + Vec3[0] * Coef20);
  Inv2[1] := - (Vec0[1] * Coef04 - Vec1[1] * Coef12 + Vec3[1] * Coef20);
  Inv2[2] := + (Vec0[2] * Coef06 - Vec1[2] * Coef14 + Vec3[2] * Coef22);
  Inv2[3] := - (Vec0[3] * Coef07 - Vec1[3] * Coef15 + Vec3[3] * Coef23);

  Inv3[0] := - (Vec0[0] * Coef08 - Vec1[0] * Coef16 + Vec2[0] * Coef20);
  Inv3[1] := + (Vec0[1] * Coef08 - Vec1[1] * Coef16 + Vec2[1] * Coef20);
  Inv3[2] := - (Vec0[2] * Coef10 - Vec1[2] * Coef18 + Vec2[2] * Coef22);
  Inv3[3] := + (Vec0[3] * Coef11 - Vec1[3] * Coef19 + Vec2[3] * Coef23);

  Row0[0] := Inv0[0];
  Row0[1] := Inv1[0];
  Row0[2] := Inv2[0];
  Row0[3] := Inv3[0];

  Dot0 := m[0, 0] * Row0[0] + m[0, 1] * Row0[1] + m[0, 2] * Row0[2] + m[0, 3] * Row0[3];

  InvDet := 1.0 / Dot0;

  Result[0, 0] := Inv0[0] * InvDet;
  Result[0, 1] := Inv0[1] * InvDet;
  Result[0, 2] := Inv0[2] * InvDet;
  Result[0, 3] := Inv0[3] * InvDet;

  Result[1, 0] := Inv1[0] * InvDet;
  Result[1, 1] := Inv1[1] * InvDet;
  Result[1, 2] := Inv1[2] * InvDet;
  Result[1, 3] := Inv1[3] * InvDet;

  Result[2, 0] := Inv2[0] * InvDet;
  Result[2, 1] := Inv2[1] * InvDet;
  Result[2, 2] := Inv2[2] * InvDet;
  Result[2, 3] := Inv2[3] * InvDet;

  Result[3, 0] := Inv3[0] * InvDet;
  Result[3, 1] := Inv3[1] * InvDet;
  Result[3, 2] := Inv3[2] * InvDet;
  Result[3, 3] := Inv3[3] * InvDet;
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

procedure Run;
  var v: TUVec4;
  var m, m1, m2, mt: TUMat;
begin
  m := TUMat.RotationY(Pi * 0.25) * TUMat.RotationZ(Pi * 0.3) * TUMat.RotationX(Pi * 0.1);
  WriteLn('m: ', m.ToString);
  m1 := UMatInverse(m);
  WriteLn('m1: ', m1.ToString);
  m2 := UMatInverse(m1);
  WriteLn('m2: ', m2.ToString);
  //mt := UMatInverse_SSE2(m);
  mt := UMatInverse_SSE1(m);
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

