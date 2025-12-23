module Complex32 = {
    type t = (f32, f32)  -- (real, imag)

    def re ((a, _): t): f32 = a
    def im ((_, b): t): f32 = b

    def one: t = (1.0f32, 0.0f32)

    def zero: t = (0.0f32, 0.0f32)

    def i: t = (0.0f32, 1.0f32)

    def scale (k: f32) (u: t): t = (k * re u, k * im u)

    def neg (u: t): t = scale (-1.0f32) u

    def add (u: t) (v: t): t = (re u + re v, im u + im v)

    def sub (u: t) (v: t): t = add u <| neg v

    def mul (u: t) (v: t): t =
        (re u * re v - im u * im v, re u * im v + im u * re v)

    def conj (u: t): t = (re u, -im u)

    def mag2 (u: t): f32 = re u * re u + im u * im u

    def mag (u: t): f32 =
        let x = f32.abs (re u)
        let y = f32.abs (im u)
        let m = f32.max x y
        in if m == 0.0f32 then 0.0f32
            else
            let xr = x / m
            let yr = y / m
            in m * f32.sqrt (xr*xr + yr*yr)

    def phase (u: t): f32 = f32.atan2 (im u) (re u)

    def exp (u: t): t =
        let e = f32.exp (re u)
        in (e * f32.cos (im u), e * f32.sin (im u))

    -- log/pow use principal branch via atan2.
    def log (u: t): t =
        let r = mag u
        let theta = phase u
        in (f32.log r, theta)

    def pow (u: t) (v: t): t =
        exp (mul v (log u))

    -- inv(0) follows IEEE float semantics (inf/nan).
    def inv (u: t): t = scale (1.0f32 / mag2 u) (conj u)

    -- div by zero follows IEEE float semantics (inf/nan).
    def div (u: t) (v: t): t = mul u (inv v)

    def fromPolar (r: f32) (theta: f32): t =
        (r * f32.cos theta, r * f32.sin theta)

}



