use std::fmt::Display;

/// Represents any mathematical expression
#[derive(Debug)]
enum Expr {
    /// A constant expression. e.g. 5, (13/4), pi, sqrt(2), (2 + 2i)
    Constant(Complex),
    /// A variable. e.g. "a", "x", "y", "x_1"
    Variable(String),
    /// A product of two expressions.
    Product(Box<Expr>, Box<Expr>),
    /// A sum of two expressions.
    Sum(Box<Expr>, Box<Expr>),
    /// An exponential expression with a base and exponent.
    Exponent(Box<Expr>, Box<Expr>),
}

/// Represents a complex number.
/// Mathematically equivalent to (a + bi) where a, b are real numbers.
#[derive(Debug)]
struct Complex {
    real: Real,
    imag: Real,
}

impl Display for Complex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Real part only, omit imaginary component
        if self.is_real() {
            return write!(f, "{}", self.real);
        }

        write!(f, "({} + {}i)", self.real, self.imag)
    }
}

impl Complex {
    /// Creates a new complex number provided the real and imaginary parts.
    fn new(real: Real, imag: Real) -> Self {
        Self { real, imag }
    }

    /// Shorthand for new(value, ZERO)
    fn real(value: Real) -> Self {
        Self::new(value, ZERO)
    }

    /// Returns whether the complex number is also real (imaginary part is zero).
    fn is_real(&self) -> bool {
        self.imag == ZERO
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

/// 0 constant
const ZERO: Real = Real::int(0);

#[derive(Debug, Clone)]
enum Real {
    Rational { num: i64, denom: i64 },
    Irrational(IrrationalKind),
}

#[derive(Debug, Clone, PartialEq)]
enum IrrationalKind {
    PI,
    E,
}

impl ToString for IrrationalKind {
    fn to_string(&self) -> String {
        match self {
            PI => "pi".to_string(),
            E => "e".to_string(),
        }
    }
}

impl PartialEq for Real {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Cross-multiply
            (Real::Rational { num: n1, denom: d1 }, Real::Rational { num: n2, denom: d2 }) => {
                n1 * d2 == n2 * d1
            }

            // Rational never equals Irrational
            (Real::Rational { .. }, Real::Irrational { .. })
            | (Real::Irrational { .. }, Real::Rational { .. }) => false,

            // For now, simply compare the "kind" of the irrational number
            (Real::Irrational(kind), Real::Irrational(other_kind)) => kind == other_kind,
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

            Self::Irrational(kind) => write!(f, "{}", kind.to_string()),
        }
    }
}

// TODO: use a better algorithm to solve this
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
    /// Used to create a real, rational number.
    /// Simplifies the numerator and denominator passed in.
    fn rational(num: i64, denom: i64) -> Self {
        let (n, d) = Self::simplify_rational(num, denom);
        Self::Rational { num: n, denom: d }
    }

    /// Used to create an integer.
    const fn int(value: i64) -> Self {
        Self::Rational {
            num: value,
            denom: 1,
        }
    }

    /// Returns whether the real number is equal to zero (ZERO constant)
    fn is_zero(&self) -> bool {
        *self == ZERO
    }

    /// Used to simplify rational numbers.
    // TODO: figure out a better API for this
    fn simplify_rational(num: i64, denom: i64) -> (i64, i64) {
        let gcd = gcd(num, denom);
        let sign = if denom < 0 { -1 } else { 1 };
        (sign * num / gcd, sign * denom.abs() / gcd)
    }

    /// Used to add two real numbers
    fn add(&self, other: &Self) -> Expr {
        match (self, other) {
            // Rational + Rational
            // (a/b) + (c/d)
            (Real::Rational { num: a, denom: b }, Real::Rational { num: c, denom: d }) => {
                let num = a * d + b * c;
                let denom = b * d;
                Expr::Constant(Complex::new(Real::rational(num, denom), ZERO))
            }

            // Rational + Irrational or Irrational + Rational
            // cannot be expressed using a single real number
            _ => {
                let self_expr = Expr::Constant(Complex::real(self.clone()));
                let other_expr = Expr::Constant(Complex::real(other.clone()));
                Expr::Sum(Box::new(self_expr), Box::new(other_expr))
            }
        }
    }

    /// Used to subtract two real numbers
    fn subtract(&self, other: &Self) -> Expr {
        todo!()
    }

    /// Used to multiply two real numbers
    fn multiply(&self, other: &Self) -> Expr {
        match (self, other) {
            // Rational * Rational
            // (a/b) * (c/d)
            (Self::Rational { num: a, denom: b }, Self::Rational { num: c, denom: d }) => {
                Expr::Constant(Complex::real(Real::rational(a * c, b * d)))
            }

            // Rational * Irrational
            // cannot be expressed using a single real number
            (Self::Rational { .. }, Self::Irrational(..))
            | (Self::Irrational(..), Self::Rational { .. }) => {
                let self_expr = Expr::Constant(Complex::real(self.clone()));
                let other_expr = Expr::Constant(Complex::real(other.clone()));
                Expr::Product(Box::new(self_expr), Box::new(other_expr))
            }

            // Irrational * Irrational
            (Self::Irrational(self_kind), Self::Irrational(other_kind)) => {
                // may be expressed as an exponential if the two irrational numbers multiplied are equal
                // e.g. pi * pi = pi^2
                if self_kind == other_kind {
                    let kind_expr = Expr::Constant(Complex::real(self.clone()));
                    let two_expr = Expr::Constant(Complex::real(Real::int(2)));
                    return Expr::Exponent(Box::new(kind_expr), Box::new(two_expr));
                }

                // The two irrational numbers aren't equal, cannot simplify
                let self_expr = Expr::Constant(Complex::real(self.clone()));
                let other_expr = Expr::Constant(Complex::real(other.clone()));
                Expr::Product(Box::new(self_expr), Box::new(other_expr))
            }
        }
    }

    /// Used to divide two real numbers
    fn divide(&self, other: &Self) -> Expr {
        match (self, other) {
            (Real::Rational { num: a, denom: b }, Real::Rational { num: c, denom: d }) => {
                // Multiply by the inverse
                Real::rational(a * d, b * c)
            }
            _ => todo!(),
        }
    }
}

fn main() {
    let a = Complex::new(Real::int(5), Real::int(0));
    let b = Complex::new(Real::rational(3, 10), Real::int(0));

    println!("{} * {} = {}", a, b, a.multiply(&b));
    println!("{:?} * {:?} = {:?}", a, b, a.multiply(&b));
}
