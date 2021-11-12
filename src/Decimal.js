const Decimal = require("decimal.js");

exports.parseDecimalImpl = Left => Right => s => {
  try {
    return Right(new Decimal(s));
  } catch (e) {
    return Left(e);
  }
};

exports.parseDecimalUnsafe = s => new Decimal(s);

exports.toString = d => d.toString();

exports.toNumber = d => d.toNumber();

exports.greaterThan = d => e => d.greaterThan(e);

exports.equals = d => e => d.equals(e);

exports.plus = d => e => d.plus(e);

exports.minus = d => e => d.minus(e);

exports.times = d => e => d.times(e);

exports.dividedBy = d => e => d.dividedBy(e);

exports.floor = d => d.floor();

exports.pow = d => e => d.pow(e);

exports.mod = d => e => d.mod(e);

exports.toDecimalPlaces = n => d => d.toDecimalPlaces(n);
