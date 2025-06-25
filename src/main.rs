use std::fmt::Display;

#[derive(Debug)]
enum Expr {
    Constant(Complex),
    Variable(String),
    Product(Box<Expr>, Box<Expr>),
    Sum(Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
struct Complex {
    real: Real,
    imag: Real,
}
impl Display for Complex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Real part only, omit imaginary component
        if self.imag == Real::zero() {
            return write!(f, "{}", self.real);
        }

        write!(f, "({} + {}i)", self.real, self.imag)
    }
}
impl Complex {
    fn new(real: Real, imag: Real) -> Self {
        Self { real, imag }
    }

    fn multiply(&self, other: &Self) -> Self {
        // (a + bi)(c + di) = (ac - bd) + (ad + bc)i
        let ac = self.real.multiply(&other.real);
        let bd = self.imag.multiply(&other.imag);
        let ad = self.real.multiply(&other.imag);
        let bc = self.imag.multiply(&other.real);

        Complex {
            real: ac.subtract(&bd),
            imag: ad.add(&bc),
        }
    }
}

#[derive(Debug)]
enum Real {
    Rational { num: i64, denom: i64 },
    Irrational,
}
impl PartialEq for Real {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Cross-multiply
            (Real::Rational { num: n1, denom: d1 }, Real::Rational { num: n2, denom: d2 }) => {
                n1 * d2 == n2 * d1
            }
            // Rational never equals Irrational
            (Real::Rational { num: _, denom: _ }, Real::Irrational)
            | (Real::Irrational, Real::Rational { num: _, denom: _ }) => false,
            _ => todo!(),
        }
    }
}
impl Display for Real {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Rational { num, denom } => {
                // Omit denominator when it's 1
                if *denom == 1 {
                    return write!(f, "{}", num);
                }

                // Write rational number as a fraction
                write!(f, "({} / {})", num, denom)
            }
            Self::Irrational => todo!(),
        }
    }
}
fn gcd(a: i64, b: i64) -> i64 {
    let mut a = a.abs();
    let mut b = b.abs();

    while b != 0 {
        let temp = b;
        b = a % b;
        a = temp;
    }

    a
}

impl Real {
    // TODO: handle this better without copying??? (keep as constant)
    fn zero() -> Self {
        Self::int(0)
    }
    fn rational(num: i64, denom: i64) -> Self {
        let (n, d) = Self::simplify_rational(num, denom);
        Self::Rational { num: n, denom: d }
    }
    fn int(value: i64) -> Self {
        Self::Rational {
            num: value,
            denom: 1,
        }
    }

    pub fn simplify_rational(num: i64, denom: i64) -> (i64, i64) {
        let gcd = gcd(num, denom);
        let sign = if denom < 0 { -1 } else { 1 };
        (sign * num / gcd, sign * denom.abs() / gcd)
    }

    fn add(&self, other: &Self) -> Self {
        match (self, other) {
            (Real::Rational { num: a, denom: b }, Real::Rational { num: c, denom: d }) => {
                let num = a * d + b * c;
                let denom = b * d;
                Real::rational(num, denom)
            }
            _ => todo!(),
        }
    }

    fn subtract(&self, other: &Self) -> Self {
        self.add(&other.negate())
    }

    fn multiply(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Rational { num: a, denom: b }, Self::Rational { num: c, denom: d }) => {
                Real::rational(a * c, b * d)
            }
            _ => todo!("Operations on irrational numbers not yet supported"),
        }
    }
    fn divide(&self, other: &Self) -> Self {
        match (self, other) {
            (Real::Rational { num: a, denom: b }, Real::Rational { num: c, denom: d }) => {
                // Multiply by the inverse
                Real::rational(a * d, b * c)
            }
            _ => todo!(),
        }
    }

    /// Returns the value * -1
    fn negate(&self) -> Self {
        match self {
            Real::Rational { num, denom } => Real::rational(-num, *denom),
            Real::Irrational => todo!(),
        }
    }
}

fn main() {
    let a = Complex::new(Real::int(5), Real::int(0));
    let b = Complex::new(Real::rational(3, 10), Real::int(0));

    println!("{} * {} = {}", a, b, a.multiply(&b));
    println!("{:?} * {:?} = {:?}", a, b, a.multiply(&b));
}
