module Complex64 = {
    type t = (f64, f64)  -- (real, imag)

    def re ((a, _): t): f64 = a
    def im ((_, b): t): f64 = b

    def one: t = (1.0f64, 0.0f64)

    def zero: t = (0.0f64, 0.0f64)

    def i: t = (0.0f64, 1.0f64)

    def scale (k: f64) (u: t): t = (k * re u, k * im u)

    def neg (u: t): t = scale (-1.0f64) u

    def add (u: t) (v: t): t = (re u + re v, im u + im v)

    def sub (u: t) (v: t): t = add u <| neg v

    def mul (u: t) (v: t): t =
        (re u * re v - im u * im v, re u * im v + im u * re v)

    def conj (u: t): t = (re u, -im u)

    def mag2 (u: t): f64 = re u * re u + im u * im u

    def mag (u: t): f64 =
        let x = f64.abs (re u)
        let y = f64.abs (im u)
        let m = f64.max x y
        in if m == 0.0f64 then 0.0f64
            else
            let xr = x / m
            let yr = y / m
            in m * f64.sqrt (xr*xr + yr*yr)

    def phase (u: t): f64 = f64.atan2 (im u) (re u)

    def exp (u: t): t =
        let e = f64.exp (re u)
        in (e * f64.cos (im u), e * f64.sin (im u))

    -- log/pow use principal branch via atan2.
    def log (u: t): t =
        let r = mag u
        let theta = phase u
        in (f64.log r, theta)

    def pow (u: t) (v: t): t =
        exp (mul v (log u))

    -- inv(0) follows IEEE float semantics (inf/nan).
    def inv (u: t): t = scale (1.0f64 / mag2 u) (conj u)

    -- div by zero follows IEEE float semantics (inf/nan).
    def div (u: t) (v: t): t = mul u (inv v)

    def fromPolar (r: f64) (theta: f64): t =
        (r * f64.cos theta, r * f64.sin theta)

}
