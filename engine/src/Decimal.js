import Decimal from "decimal.js";

export const parseDecimalImpl = (Left) => (Right) => (s) => {
  try {
    return Right(new Decimal(s));
  } catch (e) {
    return Left(e);
  }
};

export const parseDecimalUnsafe = (s) => new Decimal(s);

export const toString = (d) => d.toString();

export const toNumber = (d) => d.toNumber();

export const greaterThan = (d) => (e) => d.greaterThan(e);

export const equals = (d) => (e) => d.equals(e);

export const plus = (d) => (e) => d.plus(e);

export const minus = (d) => (e) => d.minus(e);

export const times = (d) => (e) => d.times(e);

export const dividedBy = (d) => (e) => d.dividedBy(e);

export const floor = (d) => d.floor();

export const pow = (d) => (e) => d.pow(e);

export const mod = (d) => (e) => d.mod(e);

export const toDecimalPlaces = (n) => (d) => d.toDecimalPlaces(n);
