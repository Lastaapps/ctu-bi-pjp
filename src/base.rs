
use either::Either;

use crate::errors::MilaError;

pub type Outcome<T> = Result<T, MilaError>;

