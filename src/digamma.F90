! based on https://www.netlib.org/specfun/psi
module fazang_digamma_mod
  use, intrinsic :: iso_fortran_env
  !----------------------------------------------------------------------
  !
  ! This function program evaluates the logarithmic derivative of the
  !   gamma function, 
  !
  !      psi(x) = d/dx (gamma(x)) / gamma(x) = d/dx (ln gamma(x))
  !
  !   for real x, where either
  !
  !          -xmax1 < x < -xmin (x not a negative integer), or
  !            xmin < x.
  !
  !   The calling sequence for this function is 
  !
  !                  Y = PSI(X)
  !
  !   The main computation uses rational Chebyshev approximations
  !   published in Math. Comp. 27, 123-127 (1973) by Cody, Strecok and
  !   Thacher.  This transportable program is patterned after the
  !   machine-dependent FUNPACK program PSI(X), but cannot match that
  !   version for efficiency or accuracy.  This version uses rational
  !   approximations that are theoretically accurate to 20 significant
  !   decimal digits.  The accuracy achieved depends on the arithmetic
  !   system, the compiler, the intrinsic functions, and proper selection
  !   of the machine-dependent constants.
  !
  !*******************************************************************
  !*******************************************************************
  !
  ! Explanation of machine-dependent constants
  !
  !   XINF   = largest positive machine number
  !   XMAX1  = beta ** (p-1), where beta is the radix for the
  !            floating-point system, and p is the number of base-beta
  !            digits in the floating-point significand.  This is an
  !            upper bound on non-integral floating-point numbers, and
  !            the negative of the lower bound on acceptable negative
  !            arguments for PSI.  If rounding is necessary, round this
  !            value down.
  !   XMIN1  = the smallest in magnitude acceptable argument.  We
  !            recommend XMIN1 = MAX(1/XINF,xmin) rounded up, where
  !            xmin is the smallest positive floating-point number.
  !   XSMALL = absolute argument below which  PI*COTAN(PI*X)  may be
  !            represented by 1/X.  We recommend XSMALL < sqrt(3 eps)/pi,
  !            where eps is the smallest positive number such that
  !            1+eps > 1. 
  !   XLARGE = argument beyond which PSI(X) may be represented by
  !            LOG(X).  The solution to the equation
  !               x*ln(x) = beta ** p
  !            is a safe value.
  !
  !     Approximate values for some important machines are
  !
  !                        beta  p     eps     xmin       XINF  
  !
  !  CDC 7600      (S.P.)    2  48  7.11E-15  3.13E-294  1.26E+322
  !  CRAY-1        (S.P.)    2  48  7.11E-15  4.58E-2467 5.45E+2465
  !  IEEE (IBM/XT,
  !    SUN, etc.)  (S.P.)    2  24  1.19E-07  1.18E-38   3.40E+38
  !  IEEE (IBM/XT,
  !    SUN, etc.)  (D.P.)    2  53  1.11D-16  2.23E-308  1.79D+308
  !  IBM 3033      (D.P.)   16  14  1.11D-16  5.40D-79   7.23D+75
  !  SUN 3/160     (D.P.)    2  53  1.11D-16  2.23D-308  1.79D+308
  !  VAX 11/780    (S.P.)    2  24  5.96E-08  2.94E-39   1.70E+38
  !                (D.P.)    2  56  1.39D-17  2.94D-39   1.70D+38
  !   (G Format)   (D.P.)    2  53  1.11D-16  5.57D-309  8.98D+307
  !
  !                         XMIN1      XMAX1     XSMALL    XLARGE
  !
  !  CDC 7600      (S.P.)  3.13E-294  1.40E+14  4.64E-08  9.42E+12
  !  CRAY-1        (S.P.)  1.84E-2466 1.40E+14  4.64E-08  9.42E+12
  !  IEEE (IBM/XT,
  !    SUN, etc.)  (S.P.)  1.18E-38   8.38E+06  1.90E-04  1.20E+06
  !  IEEE (IBM/XT,
  !    SUN, etc.)  (D.P.)  2.23D-308  4.50D+15  5.80D-09  2.71D+14
  !  IBM 3033      (D.P.)  1.39D-76   4.50D+15  5.80D-09  2.05D+15
  !  SUN 3/160     (D.P.)  2.23D-308  4.50D+15  5.80D-09  2.71D+14
  !  VAX 11/780    (S.P.)  5.89E-39   8.38E+06  1.35E-04  1.20E+06
  !                (D.P.)  5.89D-39   3.60D+16  2.05D-09  2.05D+15
  !   (G Format)   (D.P.)  1.12D-308  4.50D+15  5.80D-09  2.71D+14
  !
  !*******************************************************************
  !*******************************************************************
  !
  ! Error Returns
  !
  !  The program returns XINF for  X < -XMAX1, for X zero or a negative
  !    integer, or when X lies in (-XMIN1, 0), and returns -XINF
  !    when X lies in (0, XMIN1).
  !
  ! Intrinsic functions required are:
  !
  !     ABS, AINT, DBLE, INT, LOG, REAL, TAN
  !
  !
  !  Author: W. J. Cody
  !          Mathematics and Computer Science Division 
  !          Argonne National Laboratory
  !          Argonne, IL 60439
  !
  !  Latest modification: June 8, 1988
  !
  !----------------------------------------------------------------------
  private
  public :: digamma

  real(real64), parameter :: piov4 = 7.8539816339744830962D-01

  ! p1 & q1: Coefficients for approximation to  psi(x)/(x-x0)  over [0.5, 3.0]
  real(real64), parameter :: p1(9) = &
       [4.5104681245762934160D-03,5.4932855833000385356D+00, &
       3.7646693175929276856D+02,7.9525490849151998065D+03,  &
       7.1451595818951933210D+04,3.0655976301987365674D+05,  &
       6.3606997788964458797D+05,5.8041312783537569993D+05,  &
       1.6585695029761022321D+05]
  real(real64), parameter :: q1(8) = &
       [9.6141654774222358525D+01,2.6287715790581193330D+03, &
       2.9862497022250277920D+04,1.6206566091533671639D+05, &
       4.3487880712768329037D+05,5.4256384537269993733D+05, &
       2.4242185002017985252D+05,6.4155223783576225996D-08]

  ! Coefficients for approximation to  psi(x) - ln(x) + 1/(2x) for  x > 3.0
  real(real64), parameter :: p2(7) = &
       [-2.7103228277757834192D+00,-1.5166271776896121383D+01,&
       -1.9784554148719218667D+01,-8.8100958828312219821D+00, &
       -1.4479614616899842986D+00,-7.3689600332394549911D-02, &
       -6.5135387732718171306D-21]
  real(real64), parameter :: q2(6) = &
       [4.4992760373789365846D+01, 2.0240955312679931159D+02,&
       2.4736979003315290057D+02, 1.0742543875702278326D+02, &
       1.7463965060678569906D+01, 8.8427520398873480342D-01]

  !  Zero of psi(x)
  real(real64), parameter :: X01 =187.0D0, X01D = 128.0D0, X02 = 6.9464496836234126266D-04

  !  Machine-dependent constants
  real(real64), parameter :: XINF=1.70D+38, XMIN1=5.89D-39, XMAX1=3.60D+16
  real(real64), parameter :: XSMALL=2.05D-09, XLARGE=2.04D+15

contains
  elemental function digamma(xx) result(psi)
    real(real64), intent(in) :: XX
    real(real64) :: sgn, upper, w, x, z
    real(real64) :: aug,den,psi
    integer i,n,nq
    X = XX
    W = ABS(X)
    AUG = 0.D0
!----------------------------------------------------------------------
!  Check for valid arguments, then branch to appropriate algorithm
!----------------------------------------------------------------------
    IF ((-X .GE. XMAX1) .OR. (W .LT. XMIN1)) THEN
       GO TO 410
    ELSE IF (X .GE. 0.5D0) THEN
       GO TO 200
!----------------------------------------------------------------------
!  X < 0.5, use reflection formula: psi(1-x) = psi(x) + pi * cot(pi*x)
!     Use 1/X for PI*COTAN(PI*X)  when  XMIN1 < |X| <= XSMALL.  
!----------------------------------------------------------------------
    ELSE IF (W .LE. XSMALL) THEN
       AUG = -1.D0 / X
       GO TO 150
    END IF
!----------------------------------------------------------------------
!  Argument reduction for cot
!----------------------------------------------------------------------
    IF (X .LT. 0.D0) THEN
       SGN = PIOV4
    ELSE
       SGN = -PIOV4
    END IF
    W = W - AINT(W)
    NQ = INT(W * 4.D0)
    W = 4.D0 * (W - dble(NQ) * 0.25D0)
!----------------------------------------------------------------------
!  W is now related to the fractional part of  4.0 * X.
!     Adjust argument to correspond to values in the first
!     quadrant and determine the sign.
!----------------------------------------------------------------------
    N = NQ / 2
    IF ((N+N) .NE. NQ) W = 1.D0 - W
    Z = PIOV4 * W
    IF (MOD(N,2) .NE. 0) SGN = - SGN
!----------------------------------------------------------------------
!  determine the final value for  -pi * cotan(pi*x)
!----------------------------------------------------------------------
    N = (NQ + 1) / 2
    IF (MOD(N,2) .EQ. 0) THEN
!----------------------------------------------------------------------
!  Check for singularity
!----------------------------------------------------------------------
            IF (Z .EQ. 0.D0) GO TO 410
            AUG = SGN * (4.D0 / TAN(Z))
         ELSE
            AUG = SGN * (4.D0 * TAN(Z))
      END IF
  150 X = 1.D0 - X
  200 IF (X .GT. 3.D0) GO TO 300
!----------------------------------------------------------------------
!  0.5 <= X <= 3.0
!----------------------------------------------------------------------
      DEN = X
      UPPER = P1(1) * X
      DO I = 1, 7
         DEN = (DEN + Q1(I)) * X
         UPPER = (UPPER + P1(I+1)) * X
      ENDDO
      DEN = (UPPER + P1(9)) / (DEN + Q1(8))
      X = (X-X01/X01D) - X02
      PSI = DEN * X + AUG
      GO TO 500
!----------------------------------------------------------------------
!  3.0 < X 
!----------------------------------------------------------------------
  300 IF (X .LT. XLARGE) THEN
         W = 1.D0 / (X * X)
         DEN = W
         UPPER = P2(1) * W
         DO I = 1, 5
            DEN = (DEN + Q2(I)) * W
            UPPER = (UPPER + P2(I+1)) * W
         ENDDO
         AUG = (UPPER + P2(7)) / (DEN + Q2(6)) - 0.5D0 / X + AUG
      END IF
      PSI = AUG + LOG(X)
      GO TO 500
!----------------------------------------------------------------------
!  Error return
!----------------------------------------------------------------------
  410 PSI = XINF
      IF (X .GT. 0.D0) PSI = -XINF
  500 RETURN

    end function digamma
  end module fazang_digamma_mod
