infix operator %= {
    associativity right
    precedence 90
    assignment
}

infix operator >= {
    associativity none
    precedence 130
}

infix operator != {
    associativity none
    precedence 130
}

infix operator <<= {
    associativity right
    precedence 90
    assignment
}

infix operator || {
    associativity left
    precedence 110
}

infix operator += {
    associativity right
    precedence 90
    assignment
}

infix operator / {
    associativity left
    precedence 150
}

infix operator - {
    associativity left
    precedence 140
}

infix operator << {
    associativity none
    precedence 160
}

infix operator |= {
    associativity right
    precedence 90
    assignment
}

infix operator >>= {
    associativity right
    precedence 90
    assignment
}

infix operator <= {
    associativity none
    precedence 130
}

infix operator -= {
    associativity right
    precedence 90
    assignment
}

infix operator ?? {
    associativity right
    precedence 131
}

infix operator > {
    associativity none
    precedence 130
}

infix operator ~= {
    associativity none
    precedence 130
}

infix operator + {
    associativity left
    precedence 140
}

infix operator ^ {
    associativity left
    precedence 140
}

infix operator ... {
    associativity none
    precedence 135
}

infix operator /= {
    associativity right
    precedence 90
    assignment
}

infix operator * {
    associativity left
    precedence 150
}

infix operator *= {
    associativity right
    precedence 90
    assignment
}

infix operator & {
    associativity left
    precedence 150
}

infix operator !== {
    associativity none
    precedence 130
}

infix operator % {
    associativity left
    precedence 150
}

infix operator && {
    associativity left
    precedence 120
}

infix operator &* {
    associativity left
    precedence 150
}

infix operator &+ {
    associativity left
    precedence 140
}

infix operator &= {
    associativity right
    precedence 90
    assignment
}

infix operator >> {
    associativity none
    precedence 160
}

infix operator &- {
    associativity left
    precedence 140
}

infix operator === {
    associativity none
    precedence 130
}

infix operator ~> {
    associativity left
    precedence 255
}

infix operator ..< {
    associativity none
    precedence 135
}

infix operator < {
    associativity none
    precedence 130
}

infix operator == {
    associativity none
    precedence 130
}

infix operator ^= {
    associativity right
    precedence 90
    assignment
}

infix operator | {
    associativity left
    precedence 140
}

prefix operator ! {
}

prefix operator + {
}

prefix operator ~ {
}

prefix operator -- {
}

prefix operator - {
}

prefix operator ++ {
}

postfix operator ++ {
}

postfix operator -- {
}

/// Return the result of inverting `a`'s logic value.
@warn_unused_result
prefix public func !<T : BooleanType>(a: T) -> Bool

@warn_unused_result
prefix public func !(a: Bool) -> Bool

public func !=(lhs: Int32, rhs: Int32) -> Bool

@warn_unused_result
public func !=<T : Equatable>(lhs: T, rhs: T) -> Bool

/// Returns true if the arrays do not contain the same elements.
@warn_unused_result
public func !=<Element : Equatable>(lhs: ContiguousArray<Element>, rhs: ContiguousArray<Element>) -> Bool

/// Returns true if the arrays do not contain the same elements.
@warn_unused_result
public func !=<Element : Equatable>(lhs: ArraySlice<Element>, rhs: ArraySlice<Element>) -> Bool

@warn_unused_result
public func !=<T>(lhs: _OptionalNilComparisonType, rhs: T?) -> Bool

/// Returns true if the arrays do not contain the same elements.
@warn_unused_result
public func !=<Element : Equatable>(lhs: [Element], rhs: [Element]) -> Bool

/// Return `false` iff `t0` is identical to `t1`; i.e. if they are both
/// `nil` or they both represent the same type.
@warn_unused_result
public func !=(t0: Any.Type?, t1: Any.Type?) -> Bool

/// Returns `true` iff `lhs.rawValue != rhs.rawValue`.
@warn_unused_result
public func !=<T : RawRepresentable where T.RawValue : Equatable>(lhs: T, rhs: T) -> Bool

/// Returns `true` iff `lhs.rawValue != rhs.rawValue`.
@warn_unused_result
public func !=<T : Equatable where T : RawRepresentable, T.RawValue : Equatable>(lhs: T, rhs: T) -> Bool

public func !=(lhs: UInt8, rhs: UInt8) -> Bool

@warn_unused_result
public func !=<T>(lhs: T?, rhs: _OptionalNilComparisonType) -> Bool

@warn_unused_result
public func !=<T : Equatable>(lhs: T?, rhs: T?) -> Bool

@warn_unused_result
public func !=<Key : Equatable, Value : Equatable>(lhs: [Key : Value], rhs: [Key : Value]) -> Bool

public func !=(lhs: Int8, rhs: Int8) -> Bool

public func !=(lhs: UInt16, rhs: UInt16) -> Bool

public func !=(lhs: Int16, rhs: Int16) -> Bool

public func !=(lhs: UInt32, rhs: UInt32) -> Bool

@warn_unused_result
public func !=(lhs: Float80, rhs: Float80) -> Bool

public func !=(lhs: UInt64, rhs: UInt64) -> Bool

public func !=(lhs: Int64, rhs: Int64) -> Bool

@warn_unused_result
public func !=(lhs: Double, rhs: Double) -> Bool

@warn_unused_result
public func !=(lhs: Float, rhs: Float) -> Bool

public func !=(lhs: Int, rhs: Int) -> Bool

public func !=(lhs: UInt, rhs: UInt) -> Bool

@warn_unused_result
public func !==(lhs: AnyObject?, rhs: AnyObject?) -> Bool

/// Returns false iff `lhs` and `rhs` store the same underlying collection.
@warn_unused_result
public func !==<L : AnyCollectionType, R : AnyCollectionType>(lhs: L, rhs: R) -> Bool

public func %(lhs: Int, rhs: Int) -> Int

@warn_unused_result
public func %(lhs: Float, rhs: Float) -> Float

@warn_unused_result
public func %(lhs: Double, rhs: Double) -> Double

@warn_unused_result
public func %(lhs: Float80, rhs: Float80) -> Float80

public func %(lhs: UInt, rhs: UInt) -> UInt

/// Divide `lhs` and `rhs`, returning the remainder and trapping in case of
/// arithmetic overflow (except in -Ounchecked builds).
@warn_unused_result
public func %<T : _IntegerArithmeticType>(lhs: T, rhs: T) -> T

public func %(lhs: Int64, rhs: Int64) -> Int64

public func %(lhs: Int8, rhs: Int8) -> Int8

public func %(lhs: UInt64, rhs: UInt64) -> UInt64

public func %(lhs: Int32, rhs: Int32) -> Int32

public func %(lhs: UInt32, rhs: UInt32) -> UInt32

public func %(lhs: Int16, rhs: Int16) -> Int16

public func %(lhs: UInt16, rhs: UInt16) -> UInt16

public func %(lhs: UInt8, rhs: UInt8) -> UInt8

public func %=(inout lhs: Float, rhs: Float)

public func %=(inout lhs: Double, rhs: Double)

public func %=(inout lhs: Float80, rhs: Float80)

/// remainder `lhs` and `rhs` and store the result in `lhs`, trapping in
/// case of arithmetic overflow (except in -Ounchecked builds).
public func %=<T : _IntegerArithmeticType>(inout lhs: T, rhs: T)

public func &(lhs: Int, rhs: Int) -> Int

public func &(lhs: UInt, rhs: UInt) -> UInt

public func &(lhs: Int64, rhs: Int64) -> Int64

public func &(lhs: UInt64, rhs: UInt64) -> UInt64

public func &(lhs: Int32, rhs: Int32) -> Int32

public func &(lhs: UInt32, rhs: UInt32) -> UInt32

public func &(lhs: Int16, rhs: Int16) -> Int16

public func &(lhs: UInt16, rhs: UInt16) -> UInt16

public func &(lhs: Int8, rhs: Int8) -> Int8

public func &(lhs: UInt8, rhs: UInt8) -> UInt8

/// If `lhs` is `false`, return it.  Otherwise, evaluate `rhs` and
/// return its `boolValue`.
@warn_unused_result
public func &&<T : BooleanType, U : BooleanType>(lhs: T, @autoclosure rhs: () throws -> U) rethrows -> Bool

@warn_unused_result
public func &&<T : BooleanType>(lhs: T, @autoclosure rhs: () throws -> Bool) rethrows -> Bool

/// multiply `lhs` and `rhs`, silently discarding any overflow.
@warn_unused_result
public func &*<T : _IntegerArithmeticType>(lhs: T, rhs: T) -> T

/// add `lhs` and `rhs`, silently discarding any overflow.
@warn_unused_result
public func &+<T : _IntegerArithmeticType>(lhs: T, rhs: T) -> T

/// subtract `lhs` and `rhs`, silently discarding any overflow.
@warn_unused_result
public func &-<T : _IntegerArithmeticType>(lhs: T, rhs: T) -> T

@warn_unused_result
public func &=<T : BitwiseOperationsType>(inout lhs: T, rhs: T)

public func &=(inout lhs: Int, rhs: Int)

public func &=(inout lhs: UInt, rhs: UInt)

public func &=(inout lhs: Int64, rhs: Int64)

public func &=(inout lhs: UInt64, rhs: UInt64)

public func &=(inout lhs: Int32, rhs: Int32)

public func &=(inout lhs: UInt32, rhs: UInt32)

public func &=(inout lhs: Int16, rhs: Int16)

public func &=(inout lhs: UInt16, rhs: UInt16)

public func &=(inout lhs: Int8, rhs: Int8)

public func &=(inout lhs: UInt8, rhs: UInt8)

@warn_unused_result
public func *(lhs: Float, rhs: Float) -> Float

/// Multiply `lhs` and `rhs`, returning a result and trapping in case of
/// arithmetic overflow (except in -Ounchecked builds).
@warn_unused_result
public func *<T : _IntegerArithmeticType>(lhs: T, rhs: T) -> T

@warn_unused_result
public func *(lhs: Float80, rhs: Float80) -> Float80

@warn_unused_result
public func *(lhs: Double, rhs: Double) -> Double

public func *(lhs: UInt8, rhs: UInt8) -> UInt8

public func *(lhs: Int8, rhs: Int8) -> Int8

public func *(lhs: UInt16, rhs: UInt16) -> UInt16

public func *(lhs: Int16, rhs: Int16) -> Int16

public func *(lhs: UInt32, rhs: UInt32) -> UInt32

public func *(lhs: Int32, rhs: Int32) -> Int32

public func *(lhs: UInt64, rhs: UInt64) -> UInt64

public func *(lhs: Int, rhs: Int) -> Int

public func *(lhs: Int64, rhs: Int64) -> Int64

public func *(lhs: UInt, rhs: UInt) -> UInt

public func *=(inout lhs: UInt8, rhs: UInt8)

public func *=(inout lhs: Int8, rhs: Int8)

public func *=(inout lhs: UInt16, rhs: UInt16)

public func *=(inout lhs: Int16, rhs: Int16)

public func *=(inout lhs: UInt32, rhs: UInt32)

public func *=(inout lhs: Int32, rhs: Int32)

public func *=(inout lhs: UInt64, rhs: UInt64)

public func *=(inout lhs: Int64, rhs: Int64)

public func *=(inout lhs: UInt, rhs: UInt)

public func *=(inout lhs: Int, rhs: Int)

public func *=(inout lhs: Float, rhs: Float)

public func *=(inout lhs: Double, rhs: Double)

public func *=(inout lhs: Float80, rhs: Float80)

/// multiply `lhs` and `rhs` and store the result in `lhs`, trapping in
/// case of arithmetic overflow (except in -Ounchecked builds).
public func *=<T : _IntegerArithmeticType>(inout lhs: T, rhs: T)

public func +<T : UnsignedIntegerType>(lhs: T._DisallowMixedSignArithmetic, rhs: T) -> T

public func +<T : UnsignedIntegerType>(lhs: T, rhs: T._DisallowMixedSignArithmetic) -> T

public func +(lhs: UInt8, rhs: UInt8) -> UInt8

public func +(lhs: Int8, rhs: Int8) -> Int8

public func +(lhs: UInt16, rhs: UInt16) -> UInt16

public func +(lhs: Int16, rhs: Int16) -> Int16

public func +(lhs: UInt32, rhs: UInt32) -> UInt32

public func +(lhs: Int32, rhs: Int32) -> Int32

public func +(lhs: UInt64, rhs: UInt64) -> UInt64

public func +(lhs: Int64, rhs: Int64) -> Int64

public func +(lhs: UInt, rhs: UInt) -> UInt

public func +(lhs: Int, rhs: Int) -> Int

@warn_unused_result
prefix public func +(x: Float) -> Float

@warn_unused_result
public func +(lhs: Float, rhs: Float) -> Float

@warn_unused_result
prefix public func +(x: Double) -> Double

@warn_unused_result
public func +(lhs: Double, rhs: Double) -> Double

@warn_unused_result
prefix public func +(x: Float80) -> Float80

@warn_unused_result
public func +(lhs: Float80, rhs: Float80) -> Float80

/// Add `lhs` and `rhs`, returning a result and trapping in case of
/// arithmetic overflow (except in -Ounchecked builds).
@warn_unused_result
public func +<T : _IntegerArithmeticType>(lhs: T, rhs: T) -> T

prefix public func +<T : SignedNumberType>(x: T) -> T

@warn_unused_result
public func +(lhs: String, rhs: String) -> String

@warn_unused_result
public func +<Memory>(lhs: UnsafeMutablePointer<Memory>, rhs: Int) -> UnsafeMutablePointer<Memory>

@warn_unused_result
public func +<Memory>(lhs: Int, rhs: UnsafeMutablePointer<Memory>) -> UnsafeMutablePointer<Memory>

@warn_unused_result
public func +<T : Strideable>(lhs: T.Stride, rhs: T) -> T

@warn_unused_result
public func +<T : Strideable>(lhs: T, rhs: T.Stride) -> T

@warn_unused_result
public func +<RRC1 : RangeReplaceableCollectionType, RRC2 : RangeReplaceableCollectionType where RRC1.Generator.Element == RRC2.Generator.Element>(lhs: RRC1, rhs: RRC2) -> RRC1

@warn_unused_result
public func +<C : RangeReplaceableCollectionType, S : CollectionType where S.Generator.Element == C.Generator.Element>(lhs: C, rhs: S) -> C

@warn_unused_result
public func +<Memory>(lhs: UnsafePointer<Memory>, rhs: Int) -> UnsafePointer<Memory>

@warn_unused_result
public func +<C : RangeReplaceableCollectionType, S : SequenceType where S.Generator.Element == C.Generator.Element>(lhs: S, rhs: C) -> C

@warn_unused_result
public func +<C : RangeReplaceableCollectionType, S : SequenceType where S.Generator.Element == C.Generator.Element>(lhs: C, rhs: S) -> C

@warn_unused_result
public func +<Memory>(lhs: Int, rhs: UnsafePointer<Memory>) -> UnsafePointer<Memory>

postfix public func ++(inout x: Int) -> Int

prefix public func ++(inout x: Int) -> Int

postfix public func ++(inout x: UInt) -> UInt

prefix public func ++(inout x: UInt) -> UInt

postfix public func ++(inout x: Int64) -> Int64

prefix public func ++(inout x: Int64) -> Int64

postfix public func ++(inout x: UInt64) -> UInt64

prefix public func ++(inout x: UInt64) -> UInt64

postfix public func ++(inout x: Int32) -> Int32

prefix public func ++(inout x: Int32) -> Int32

postfix public func ++(inout x: UInt32) -> UInt32

prefix public func ++(inout x: UInt32) -> UInt32

postfix public func ++(inout x: Int16) -> Int16

prefix public func ++(inout x: Int16) -> Int16

postfix public func ++(inout x: UInt16) -> UInt16

prefix public func ++(inout x: UInt16) -> UInt16

postfix public func ++(inout x: Int8) -> Int8

prefix public func ++(inout x: Int8) -> Int8

postfix public func ++(inout x: UInt8) -> UInt8

prefix public func ++(inout x: UInt8) -> UInt8

postfix public func ++(inout lhs: Double) -> Double

postfix public func ++(inout lhs: Float) -> Float

prefix public func ++(inout rhs: Double) -> Double

prefix public func ++(inout rhs: Float80) -> Float80

postfix public func ++(inout lhs: Float80) -> Float80

/// Replace `i` with its `successor()` and return the updated value of
/// `i`.
prefix public func ++<T : _Incrementable>(inout i: T) -> T

/// Replace `i` with its `successor()` and return the original
/// value of `i`.
postfix public func ++<T : _Incrementable>(inout i: T) -> T

prefix public func ++(inout rhs: Float) -> Float

public func +=<T : Strideable>(inout lhs: T, rhs: T.Stride)

/// add `lhs` and `rhs` and store the result in `lhs`, trapping in
/// case of arithmetic overflow (except in -Ounchecked builds).
public func +=<T : _IntegerArithmeticType>(inout lhs: T, rhs: T)

public func +=(inout lhs: Float80, rhs: Float80)

public func +=<Memory>(inout lhs: UnsafePointer<Memory>, rhs: Int)

public func +=(inout lhs: Double, rhs: Double)

public func +=<Memory>(inout lhs: UnsafeMutablePointer<Memory>, rhs: Int)

public func +=(inout lhs: Float, rhs: Float)

public func +=(inout lhs: Int, rhs: Int)

public func +=(inout lhs: UInt, rhs: UInt)

public func +=(inout lhs: Int64, rhs: Int64)

public func +=(inout lhs: UInt64, rhs: UInt64)

public func +=(inout lhs: Int32, rhs: Int32)

public func +=(inout lhs: UInt32, rhs: UInt32)

public func +=(inout lhs: Int16, rhs: Int16)

public func +=(inout lhs: UInt16, rhs: UInt16)

public func +=(inout lhs: Int8, rhs: Int8)

public func +=(inout lhs: UInt8, rhs: UInt8)

/// Append the elements of `rhs` to `lhs`.
public func +=<Element, C : CollectionType where C.Generator.Element == Element>(inout lhs: _ContiguousArrayBuffer<Element>, rhs: C)

/// Extend `lhs` with the elements of `rhs`.
public func +=<Element, C : CollectionType where C.Generator.Element == Element>(inout lhs: [Element], rhs: C)

/// Extend `lhs` with the elements of `rhs`.
public func +=<Element, S : SequenceType where S.Generator.Element == Element>(inout lhs: [Element], rhs: S)

/// Extend `lhs` with the elements of `rhs`.
public func +=<Element, C : CollectionType where C.Generator.Element == Element>(inout lhs: ArraySlice<Element>, rhs: C)

/// Extend `lhs` with the elements of `rhs`.
public func +=<Element, S : SequenceType where S.Generator.Element == Element>(inout lhs: ArraySlice<Element>, rhs: S)

/// Extend `lhs` with the elements of `rhs`.
public func +=<Element, C : CollectionType where C.Generator.Element == Element>(inout lhs: ContiguousArray<Element>, rhs: C)

/// Extend `lhs` with the elements of `rhs`.
public func +=<Element, S : SequenceType where S.Generator.Element == Element>(inout lhs: ContiguousArray<Element>, rhs: S)

public func +=<T : UnsignedIntegerType>(inout lhs: T, rhs: T._DisallowMixedSignArithmetic)

public func +=(inout lhs: String, rhs: String)

public func -(lhs: Int8, rhs: Int8) -> Int8

public func -(lhs: UInt8, rhs: UInt8) -> UInt8

public func -(lhs: UInt16, rhs: UInt16) -> UInt16

public func -(lhs: Int16, rhs: Int16) -> Int16

public func -(lhs: UInt32, rhs: UInt32) -> UInt32

public func -(lhs: Int32, rhs: Int32) -> Int32

public func -(lhs: UInt64, rhs: UInt64) -> UInt64

public func -(lhs: Int64, rhs: Int64) -> Int64

public func -(lhs: UInt, rhs: UInt) -> UInt

public func -(lhs: Int, rhs: Int) -> Int

@warn_unused_result
prefix public func -(x: Float) -> Float

@warn_unused_result
public func -(lhs: Float, rhs: Float) -> Float

@warn_unused_result
prefix public func -(x: Double) -> Double

@warn_unused_result
public func -(lhs: Double, rhs: Double) -> Double

@warn_unused_result
prefix public func -(x: Float80) -> Float80

@warn_unused_result
public func -(lhs: Float80, rhs: Float80) -> Float80

/// Subtract `lhs` and `rhs`, returning a result and trapping in case of
/// arithmetic overflow (except in -Ounchecked builds).
@warn_unused_result
public func -<T : _IntegerArithmeticType>(lhs: T, rhs: T) -> T

prefix public func -<T : SignedNumberType>(x: T) -> T

@warn_unused_result
public func -<T : Strideable>(lhs: T, rhs: T.Stride) -> T

public func -<T : _DisallowMixedSignArithmetic>(lhs: T, rhs: T) -> T._DisallowMixedSignArithmetic

@warn_unused_result
public func -<T : Strideable>(lhs: T, rhs: T) -> T.Stride

public func -<T : _DisallowMixedSignArithmetic>(lhs: T, rhs: T._DisallowMixedSignArithmetic) -> T

@warn_unused_result
public func -<Memory>(lhs: UnsafeMutablePointer<Memory>, rhs: Int) -> UnsafeMutablePointer<Memory>

@warn_unused_result
public func -<Memory>(lhs: UnsafePointer<Memory>, rhs: UnsafePointer<Memory>) -> Int

@warn_unused_result
public func -<Memory>(lhs: UnsafePointer<Memory>, rhs: Int) -> UnsafePointer<Memory>

@warn_unused_result
public func -<Memory>(lhs: UnsafeMutablePointer<Memory>, rhs: UnsafeMutablePointer<Memory>) -> Int

postfix public func --(inout x: UInt32) -> UInt32

prefix public func --(inout x: Int) -> Int

prefix public func --(inout x: UInt8) -> UInt8

postfix public func --(inout x: UInt8) -> UInt8

/// Replace `i` with its `predecessor()` and return the original
/// value of `i`.
postfix public func --<T : BidirectionalIndexType>(inout i: T) -> T

/// Replace `i` with its `predecessor()` and return the updated value
/// of `i`.
prefix public func --<T : BidirectionalIndexType>(inout i: T) -> T

postfix public func --(inout lhs: Float80) -> Float80

prefix public func --(inout rhs: Float80) -> Float80

prefix public func --(inout x: Int8) -> Int8

postfix public func --(inout x: Int8) -> Int8

prefix public func --(inout x: UInt16) -> UInt16

postfix public func --(inout lhs: Double) -> Double

prefix public func --(inout rhs: Double) -> Double

postfix public func --(inout x: UInt16) -> UInt16

prefix public func --(inout x: Int16) -> Int16

postfix public func --(inout x: Int16) -> Int16

prefix public func --(inout x: UInt32) -> UInt32

postfix public func --(inout lhs: Float) -> Float

prefix public func --(inout x: Int32) -> Int32

prefix public func --(inout rhs: Float) -> Float

postfix public func --(inout x: Int32) -> Int32

prefix public func --(inout x: UInt64) -> UInt64

postfix public func --(inout x: UInt64) -> UInt64

postfix public func --(inout x: Int) -> Int

prefix public func --(inout x: Int64) -> Int64

postfix public func --(inout x: Int64) -> Int64

prefix public func --(inout x: UInt) -> UInt

postfix public func --(inout x: UInt) -> UInt

public func -=(inout lhs: Float, rhs: Float)

public func -=(inout lhs: Int, rhs: Int)

public func -=(inout lhs: UInt16, rhs: UInt16)

public func -=(inout lhs: Double, rhs: Double)

public func -=(inout lhs: Float80, rhs: Float80)

public func -=(inout lhs: UInt, rhs: UInt)

public func -=(inout lhs: Int64, rhs: Int64)

public func -=(inout lhs: UInt64, rhs: UInt64)

/// subtract `lhs` and `rhs` and store the result in `lhs`, trapping in
/// case of arithmetic overflow (except in -Ounchecked builds).
public func -=<T : _IntegerArithmeticType>(inout lhs: T, rhs: T)

public func -=(inout lhs: Int32, rhs: Int32)

public func -=(inout lhs: UInt32, rhs: UInt32)

public func -=<T : Strideable>(inout lhs: T, rhs: T.Stride)

public func -=<T : UnsignedIntegerType>(inout lhs: T, rhs: T._DisallowMixedSignArithmetic)

public func -=<Memory>(inout lhs: UnsafeMutablePointer<Memory>, rhs: Int)

public func -=<Memory>(inout lhs: UnsafePointer<Memory>, rhs: Int)

public func -=(inout lhs: UInt8, rhs: UInt8)

public func -=(inout lhs: Int8, rhs: Int8)

public func -=(inout lhs: Int16, rhs: Int16)

/// Forms a closed range that contains both `start` and `end`.
/// - Requires: `start <= end`.
@warn_unused_result
public func ...<Pos : ForwardIndexType where Pos : Comparable>(start: Pos, end: Pos) -> Range<Pos>

/// Forms a closed range that contains both `minimum` and `maximum`.
@warn_unused_result
public func ...<Pos : ForwardIndexType>(minimum: Pos, maximum: Pos) -> Range<Pos>

/// Returns a closed interval from `start` through `end`.
@warn_unused_result
public func ...<Bound : Comparable>(start: Bound, end: Bound) -> ClosedInterval<Bound>

/// Forms a half-open range that contains `start`, but not `end`.
///
/// - Requires: `start <= end`.
@warn_unused_result
public func ..<<Pos : ForwardIndexType where Pos : Comparable>(start: Pos, end: Pos) -> Range<Pos>

/// Forms a half-open range that contains `minimum`, but not
/// `maximum`.
@warn_unused_result
public func ..<<Pos : ForwardIndexType>(minimum: Pos, maximum: Pos) -> Range<Pos>

/// Returns a half-open interval from `start` to `end`.
@warn_unused_result
public func ..<<Bound : Comparable>(start: Bound, end: Bound) -> HalfOpenInterval<Bound>

/// Divide `lhs` and `rhs`, returning a result and trapping in case of
/// arithmetic overflow (except in -Ounchecked builds).
@warn_unused_result
public func /<T : _IntegerArithmeticType>(lhs: T, rhs: T) -> T

@warn_unused_result
public func /(lhs: Float80, rhs: Float80) -> Float80

@warn_unused_result
public func /(lhs: Double, rhs: Double) -> Double

@warn_unused_result
public func /(lhs: Float, rhs: Float) -> Float

public func /(lhs: Int, rhs: Int) -> Int

public func /(lhs: UInt, rhs: UInt) -> UInt

public func /(lhs: Int64, rhs: Int64) -> Int64

public func /(lhs: UInt64, rhs: UInt64) -> UInt64

public func /(lhs: Int32, rhs: Int32) -> Int32

public func /(lhs: UInt32, rhs: UInt32) -> UInt32

public func /(lhs: Int16, rhs: Int16) -> Int16

public func /(lhs: UInt16, rhs: UInt16) -> UInt16

public func /(lhs: Int8, rhs: Int8) -> Int8

public func /(lhs: UInt8, rhs: UInt8) -> UInt8

public func /=(inout lhs: Float, rhs: Float)

public func /=(inout lhs: Double, rhs: Double)

public func /=(inout lhs: Float80, rhs: Float80)

/// divide `lhs` and `rhs` and store the result in `lhs`, trapping in
/// case of arithmetic overflow (except in -Ounchecked builds).
public func /=<T : _IntegerArithmeticType>(inout lhs: T, rhs: T)

@warn_unused_result
public func <<Element : Hashable>(lhs: SetIndex<Element>, rhs: SetIndex<Element>) -> Bool

@warn_unused_result
public func <(lhs: Bit, rhs: Bit) -> Bool

@warn_unused_result
public func <<Memory>(lhs: UnsafePointer<Memory>, rhs: UnsafePointer<Memory>) -> Bool

@warn_unused_result
public func <<Memory>(lhs: UnsafeMutablePointer<Memory>, rhs: UnsafeMutablePointer<Memory>) -> Bool

@warn_unused_result
public func <(lhs: UnicodeScalar, rhs: UnicodeScalar) -> Bool

@warn_unused_result
public func <(lhs: String.UTF16View.Index, rhs: String.UTF16View.Index) -> Bool

@warn_unused_result
public func <(lhs: String.UnicodeScalarView.Index, rhs: String.UnicodeScalarView.Index) -> Bool

@warn_unused_result
public func <(lhs: Index, rhs: Index) -> Bool

@warn_unused_result
public func <(lhs: String, rhs: String) -> Bool

/// Compare two `Strideable`s.
public func <<T : _Strideable>(x: T, y: T) -> Bool

@warn_unused_result
public func <(lhs: Character, rhs: Character) -> Bool

public func <(lhs: UInt8, rhs: UInt8) -> Bool

public func <(lhs: Int8, rhs: Int8) -> Bool

public func <(lhs: UInt16, rhs: UInt16) -> Bool

@warn_unused_result
public func <(lhs: ObjectIdentifier, rhs: ObjectIdentifier) -> Bool

public func <(lhs: Int16, rhs: Int16) -> Bool

public func <(lhs: UInt32, rhs: UInt32) -> Bool

public func <(lhs: Int32, rhs: Int32) -> Bool

public func <(lhs: UInt64, rhs: UInt64) -> Bool

public func <(lhs: Int64, rhs: Int64) -> Bool

public func <(lhs: UInt, rhs: UInt) -> Bool

public func <(lhs: Int, rhs: Int) -> Bool

@warn_unused_result
public func <(lhs: Float, rhs: Float) -> Bool

@warn_unused_result
public func <<T : Comparable>(lhs: T?, rhs: T?) -> Bool

@warn_unused_result
public func <(lhs: Double, rhs: Double) -> Bool

@warn_unused_result
public func <(lhs: Float80, rhs: Float80) -> Bool

@warn_unused_result
public func <<Key : Hashable, Value>(lhs: DictionaryIndex<Key, Value>, rhs: DictionaryIndex<Key, Value>) -> Bool

public func <<(lhs: UInt64, rhs: UInt64) -> UInt64

public func <<(lhs: Int64, rhs: Int64) -> Int64

public func <<(lhs: UInt, rhs: UInt) -> UInt

public func <<(lhs: UInt8, rhs: UInt8) -> UInt8

public func <<(lhs: Int8, rhs: Int8) -> Int8

public func <<(lhs: UInt16, rhs: UInt16) -> UInt16

public func <<(lhs: Int16, rhs: Int16) -> Int16

public func <<(lhs: UInt32, rhs: UInt32) -> UInt32

public func <<(lhs: Int32, rhs: Int32) -> Int32

public func <<(lhs: Int, rhs: Int) -> Int

public func <<=(inout lhs: UInt8, rhs: UInt8)

public func <<=(inout lhs: Int8, rhs: Int8)

public func <<=(inout lhs: UInt16, rhs: UInt16)

public func <<=(inout lhs: Int16, rhs: Int16)

public func <<=(inout lhs: UInt32, rhs: UInt32)

public func <<=(inout lhs: Int32, rhs: Int32)

public func <<=(inout lhs: UInt64, rhs: UInt64)

public func <<=(inout lhs: Int64, rhs: Int64)

public func <<=(inout lhs: UInt, rhs: UInt)

public func <<=(inout lhs: Int, rhs: Int)

public func <=(lhs: UInt8, rhs: UInt8) -> Bool

@warn_unused_result
public func <=(lhs: Float, rhs: Float) -> Bool

public func <=(lhs: Int8, rhs: Int8) -> Bool

public func <=(lhs: Int32, rhs: Int32) -> Bool

public func <=(lhs: UInt16, rhs: UInt16) -> Bool

public func <=(lhs: Int16, rhs: Int16) -> Bool

public func <=(lhs: UInt64, rhs: UInt64) -> Bool

public func <=(lhs: Int64, rhs: Int64) -> Bool

public func <=(lhs: UInt, rhs: UInt) -> Bool

public func <=(lhs: Int, rhs: Int) -> Bool

@warn_unused_result
public func <=(lhs: Double, rhs: Double) -> Bool

@warn_unused_result
public func <=(lhs: Float80, rhs: Float80) -> Bool

public func <=(lhs: UInt32, rhs: UInt32) -> Bool

@warn_unused_result
public func <=<T : Comparable>(lhs: T?, rhs: T?) -> Bool

@warn_unused_result
public func <=<T : Comparable>(lhs: T, rhs: T) -> Bool

/// Returns `true` iff `lhs` is identical to `rhs`.
@warn_unused_result
public func ==<Base : CollectionType>(lhs: LazyFilterIndex<Base>, rhs: LazyFilterIndex<Base>) -> Bool

@warn_unused_result
public func ==(lhs: Bit, rhs: Bit) -> Bool

/// Returns `true` iff `lhs.rawValue == rhs.rawValue`.
@warn_unused_result
public func ==<T : RawRepresentable where T.RawValue : Equatable>(lhs: T, rhs: T) -> Bool

@warn_unused_result
public func ==<T : Equatable>(lhs: T?, rhs: T?) -> Bool

/// Returns true if these arrays contain the same elements.
@warn_unused_result
public func ==<Element : Equatable>(lhs: ArraySlice<Element>, rhs: ArraySlice<Element>) -> Bool

/// Returns true if these arrays contain the same elements.
@warn_unused_result
public func ==<Element : Equatable>(lhs: [Element], rhs: [Element]) -> Bool

public func ==<Value, Element>(lhs: ManagedBufferPointer<Value, Element>, rhs: ManagedBufferPointer<Value, Element>) -> Bool

/// Return true iff `lhs` and `rhs` wrap equal underlying
/// `AnyBidirectionalIndex`s.
///
/// - Requires: The types of indices wrapped by `lhs` and `rhs` are
///   identical.
@warn_unused_result
public func ==(lhs: AnyBidirectionalIndex, rhs: AnyBidirectionalIndex) -> Bool

/// Two `HalfOpenInterval`s are equal if their `start` and `end` are equal.
@warn_unused_result
public func ==<Bound : Comparable>(lhs: HalfOpenInterval<Bound>, rhs: HalfOpenInterval<Bound>) -> Bool

public func ==(lhs: Int64, rhs: Int64) -> Bool

@warn_unused_result
public func ==(left: _SwiftNSOperatingSystemVersion, right: _SwiftNSOperatingSystemVersion) -> Bool

/// Two `ClosedInterval`s are equal if their `start` and `end` are equal.
@warn_unused_result
public func ==<Bound : Comparable>(lhs: ClosedInterval<Bound>, rhs: ClosedInterval<Bound>) -> Bool

/// Return true iff `lhs` and `rhs` wrap equal underlying
/// `AnyRandomAccessIndex`s.
///
/// - Requires: The types of indices wrapped by `lhs` and `rhs` are
///   identical.
@warn_unused_result
public func ==(lhs: AnyRandomAccessIndex, rhs: AnyRandomAccessIndex) -> Bool

@warn_unused_result
public func ==(x: ObjectIdentifier, y: ObjectIdentifier) -> Bool

@warn_unused_result
public func ==(lhs: Bool, rhs: Bool) -> Bool

@warn_unused_result
public func ==<T>(lhs: _OptionalNilComparisonType, rhs: T?) -> Bool

@warn_unused_result
public func ==(lhs: String.UTF16View.Index, rhs: String.UTF16View.Index) -> Bool

public func ==<T : _Strideable>(x: T, y: T) -> Bool

@warn_unused_result
public func ==(lhs: String, rhs: String) -> Bool

@warn_unused_result
public func ==(lhs: String.UTF8View.Index, rhs: String.UTF8View.Index) -> Bool

@warn_unused_result
public func ==(lhs: Index, rhs: Index) -> Bool

@warn_unused_result
public func ==<Memory>(lhs: UnsafePointer<Memory>, rhs: UnsafePointer<Memory>) -> Bool

@warn_unused_result
public func ==(lhs: UnicodeScalar, rhs: UnicodeScalar) -> Bool

@warn_unused_result
public func ==<Memory>(lhs: UnsafeMutablePointer<Memory>, rhs: UnsafeMutablePointer<Memory>) -> Bool

@warn_unused_result
public func ==(lhs: String.UnicodeScalarView.Index, rhs: String.UnicodeScalarView.Index) -> Bool

@warn_unused_result
public func ==<T>(lhs: T?, rhs: _OptionalNilComparisonType) -> Bool

@warn_unused_result
public func ==<Memory>(lhs: AutoreleasingUnsafeMutablePointer<Memory>, rhs: AutoreleasingUnsafeMutablePointer<Memory>) -> Bool

/// Return true iff `lhs` and `rhs` wrap equal underlying
/// `AnyForwardIndex`s.
///
/// - Requires: The types of indices wrapped by `lhs` and `rhs` are
///   identical.
@warn_unused_result
public func ==(lhs: AnyForwardIndex, rhs: AnyForwardIndex) -> Bool

@warn_unused_result
public func ==<Value, Element>(lhs: _HeapBuffer<Value, Element>, rhs: _HeapBuffer<Value, Element>) -> Bool

@warn_unused_result
public func ==<Key : Hashable, Value>(lhs: DictionaryIndex<Key, Value>, rhs: DictionaryIndex<Key, Value>) -> Bool

@warn_unused_result
public func ==<Element>(lhs: Range<Element>, rhs: Range<Element>) -> Bool

@warn_unused_result
public func ==<Element : Hashable>(lhs: SetIndex<Element>, rhs: SetIndex<Element>) -> Bool

/// Return `true` iff `t0` is identical to `t1`; i.e. if they are both
/// `nil` or they both represent the same type.
@warn_unused_result
public func ==(t0: Any.Type?, t1: Any.Type?) -> Bool

@warn_unused_result
public func ==<Key : Equatable, Value : Equatable>(lhs: [Key : Value], rhs: [Key : Value]) -> Bool

@warn_unused_result
public func ==<Element : Hashable>(lhs: Set<Element>, rhs: Set<Element>) -> Bool

public func ==(lhs: UInt8, rhs: UInt8) -> Bool

public func ==(lhs: Int8, rhs: Int8) -> Bool

public func ==(lhs: UInt16, rhs: UInt16) -> Bool

public func ==(lhs: Int16, rhs: Int16) -> Bool

public func ==(lhs: UInt32, rhs: UInt32) -> Bool

public func ==(lhs: FloatingPointClassification, rhs: FloatingPointClassification) -> Bool

public func ==(lhs: Int32, rhs: Int32) -> Bool

@warn_unused_result
public func ==(lhs: COpaquePointer, rhs: COpaquePointer) -> Bool

@warn_unused_result
public func ==(lhs: Float80, rhs: Float80) -> Bool

public func ==(lhs: UInt64, rhs: UInt64) -> Bool

@warn_unused_result
public func ==(lhs: Double, rhs: Double) -> Bool

@warn_unused_result
public func ==(lhs: Float, rhs: Float) -> Bool

@warn_unused_result
public func ==<BaseElements>(lhs: FlattenBidirectionalCollectionIndex<BaseElements>, rhs: FlattenBidirectionalCollectionIndex<BaseElements>) -> Bool

@warn_unused_result
public func ==<BaseElements>(lhs: FlattenCollectionIndex<BaseElements>, rhs: FlattenCollectionIndex<BaseElements>) -> Bool

public func ==(lhs: Int, rhs: Int) -> Bool

public func ==(lhs: UInt, rhs: UInt) -> Bool

/// Returns true if these arrays contain the same elements.
@warn_unused_result
public func ==<Element : Equatable>(lhs: ContiguousArray<Element>, rhs: ContiguousArray<Element>) -> Bool

@warn_unused_result
public func ==<Base>(lhs: ReverseIndex<Base>, rhs: ReverseIndex<Base>) -> Bool

@warn_unused_result
public func ==(lhs: Character, rhs: Character) -> Bool

@warn_unused_result
public func ===(lhs: AnyObject?, rhs: AnyObject?) -> Bool

/// Return true iff `lhs` and `rhs` store the same underlying collection.
@warn_unused_result
public func ===<L : AnyCollectionType, R : AnyCollectionType>(lhs: L, rhs: R) -> Bool

public func >(lhs: Int, rhs: Int) -> Bool

public func >(lhs: Int8, rhs: Int8) -> Bool

@warn_unused_result
public func ><T : Comparable>(lhs: T, rhs: T) -> Bool

@warn_unused_result
public func ><T : Comparable>(lhs: T?, rhs: T?) -> Bool

@warn_unused_result
public func >(lhs: Float80, rhs: Float80) -> Bool

@warn_unused_result
public func >(lhs: Double, rhs: Double) -> Bool

@warn_unused_result
public func >(lhs: Float, rhs: Float) -> Bool

public func >(lhs: UInt, rhs: UInt) -> Bool

public func >(lhs: Int64, rhs: Int64) -> Bool

public func >(lhs: UInt64, rhs: UInt64) -> Bool

public func >(lhs: Int32, rhs: Int32) -> Bool

public func >(lhs: UInt32, rhs: UInt32) -> Bool

public func >(lhs: Int16, rhs: Int16) -> Bool

public func >(lhs: UInt16, rhs: UInt16) -> Bool

public func >(lhs: UInt8, rhs: UInt8) -> Bool

public func >=(lhs: UInt8, rhs: UInt8) -> Bool

public func >=(lhs: Int8, rhs: Int8) -> Bool

public func >=(lhs: UInt16, rhs: UInt16) -> Bool

public func >=(lhs: Int16, rhs: Int16) -> Bool

public func >=(lhs: UInt32, rhs: UInt32) -> Bool

public func >=(lhs: Int32, rhs: Int32) -> Bool

public func >=(lhs: UInt64, rhs: UInt64) -> Bool

public func >=(lhs: Int64, rhs: Int64) -> Bool

public func >=(lhs: UInt, rhs: UInt) -> Bool

public func >=(lhs: Int, rhs: Int) -> Bool

@warn_unused_result
public func >=(lhs: Float, rhs: Float) -> Bool

@warn_unused_result
public func >=(lhs: Double, rhs: Double) -> Bool

@warn_unused_result
public func >=(lhs: Float80, rhs: Float80) -> Bool

@warn_unused_result
public func >=<T : Comparable>(lhs: T?, rhs: T?) -> Bool

@warn_unused_result
public func >=<T : Comparable>(lhs: T, rhs: T) -> Bool

public func >>(lhs: Int8, rhs: Int8) -> Int8

public func >>(lhs: Int, rhs: Int) -> Int

public func >>(lhs: UInt, rhs: UInt) -> UInt

public func >>(lhs: Int64, rhs: Int64) -> Int64

public func >>(lhs: UInt64, rhs: UInt64) -> UInt64

public func >>(lhs: UInt8, rhs: UInt8) -> UInt8

public func >>(lhs: UInt16, rhs: UInt16) -> UInt16

public func >>(lhs: Int16, rhs: Int16) -> Int16

public func >>(lhs: Int32, rhs: Int32) -> Int32

public func >>(lhs: UInt32, rhs: UInt32) -> UInt32

public func >>=(inout lhs: UInt8, rhs: UInt8)

public func >>=(inout lhs: Int8, rhs: Int8)

public func >>=(inout lhs: UInt16, rhs: UInt16)

public func >>=(inout lhs: Int16, rhs: Int16)

public func >>=(inout lhs: UInt32, rhs: UInt32)

public func >>=(inout lhs: Int32, rhs: Int32)

public func >>=(inout lhs: UInt64, rhs: UInt64)

public func >>=(inout lhs: Int64, rhs: Int64)

public func >>=(inout lhs: UInt, rhs: UInt)

public func >>=(inout lhs: Int, rhs: Int)

@warn_unused_result
public func ??<T>(optional: T?, @autoclosure defaultValue: () throws -> T?) rethrows -> T?

@warn_unused_result
public func ??<T>(optional: T?, @autoclosure defaultValue: () throws -> T) rethrows -> T

/// A type that supports an "absolute value" function.
public protocol AbsoluteValuable : SignedNumberType {
    /// Returns the absolute value of `x`.
    @warn_unused_result
    public static func abs(x: Self) -> Self
}

/// The protocol to which all types implicitly conform.
public typealias Any = protocol<>

/// A type-erased wrapper over any collection with indices that
/// support bidirectional traversal.
///
/// Forwards operations to an arbitrary underlying collection having the
/// same `Element` type, hiding the specifics of the underlying
/// `CollectionType`.
///
/// - SeeAlso: `AnyRandomAccessType`, `AnyForwardType`
public struct AnyBidirectionalCollection<Element> : AnyCollectionType {
    /// Create an `AnyBidirectionalCollection` that stores `base` as its
    /// underlying collection.
    ///
    /// - Complexity: O(1).
    public init<C : CollectionType where C.Index : BidirectionalIndexType, C.Generator.Element == Element>(_ base: C)
    /// Create an `AnyBidirectionalCollection` having the same underlying
    /// collection as `other`.
    ///
    /// - Postcondition: The result is `===` to `other`.
    ///
    /// - Complexity: O(1).
    public init(_ other: AnyBidirectionalCollection<Element>)
    /// Create an `AnyBidirectionalCollection` that stores `base` as its
    /// underlying collection.
    ///
    /// - Complexity: O(1).
    public init<C : CollectionType where C.Index : RandomAccessIndexType, C.Generator.Element == Element>(_ base: C)
    /// Create an `AnyBidirectionalCollection` having the same underlying
    /// collection as `other`.
    ///
    /// - Postcondition: The result is `===` to `other`.
    ///
    /// - Complexity: O(1).
    public init(_ other: AnyRandomAccessCollection<Element>)
    /// If the indices of the underlying collection stored by `other`
    /// satisfy `BidirectionalIndexType`, create an
    /// `AnyBidirectionalCollection` having the same underlying
    /// collection as `other`.  Otherwise, the result is `nil`.
    ///
    /// - Complexity: O(1).
    public init?(_ other: AnyForwardCollection<Element>)
    /// Returns a *generator* over the elements of this *collection*.
    ///
    /// - Complexity: O(1).
    public func generate() -> AnyGenerator<Element>
    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    public var startIndex: AnyBidirectionalIndex { get }
    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    public var endIndex: AnyBidirectionalIndex { get }
    public subscript (position: AnyBidirectionalIndex) -> Element { get }
    /// Return the number of elements.
    ///
    /// - Complexity: O(N).
    public var count: IntMax { get }
}

extension AnyBidirectionalCollection {
    public func underestimateCount() -> Int
}

/// A wrapper over an underlying `BidirectionalIndexType` that hides
/// the specific underlying type.
///
/// - SeeAlso: `AnyBidirectionalCollection`
public struct AnyBidirectionalIndex : BidirectionalIndexType {
    public typealias Distance = IntMax
    /// Wrap and forward operations to `base`.
    public init<BaseIndex : BidirectionalIndexType>(_ base: BaseIndex)
    /// Return the next consecutive value in a discrete sequence of
    /// `AnyBidirectionalIndex` values.
    ///
    /// - Requires: `self` has a well-defined successor.
    public func successor() -> AnyBidirectionalIndex
    /// Return the previous consecutive value in a discrete sequence of
    /// `AnyBidirectionalIndex` values.
    ///
    /// - Requires: `self` has a well-defined predecessor.
    public func predecessor() -> AnyBidirectionalIndex
}

/// The protocol to which all class types implicitly conform.
///
/// When used as a concrete type, all known `@objc` `class` methods and
/// properties are available, as implicitly-unwrapped-optional methods
/// and properties respectively, on each instance of `AnyClass`. For
/// example:
///
///     class C {
///       @objc class var cValue: Int { return 42 }
///     }
///
///     // If x has an @objc cValue: Int, return its value.
///     // Otherwise, return nil.
///     func getCValue(x: AnyClass) -> Int? {
///       return x.cValue // <===
///     }
///
/// - SeeAlso: `AnyObject`
public typealias AnyClass = AnyObject.Type

/// A protocol for `AnyForwardCollection<Element>`,
/// `AnyBidirectionalCollection<Element>`, and
/// `AnyRandomAccessCollection<Element>`.
///
/// This protocol can be considered an implementation detail of the
/// `===` and `!==` implementations for these types.
public protocol AnyCollectionType : CollectionType {
}

/// A type-erased wrapper over any collection with indices that
/// support forward traversal.
///
/// Forwards operations to an arbitrary underlying collection having the
/// same `Element` type, hiding the specifics of the underlying
/// `CollectionType`.
///
/// - SeeAlso: `AnyBidirectionalType`, `AnyRandomAccessType`
public struct AnyForwardCollection<Element> : AnyCollectionType {
    /// Create an `AnyForwardCollection` that stores `base` as its
    /// underlying collection.
    ///
    /// - Complexity: O(1).
    public init<C : CollectionType where C.Index : ForwardIndexType, C.Generator.Element == Element>(_ base: C)
    /// Create an `AnyForwardCollection` having the same underlying
    /// collection as `other`.
    ///
    /// - Postcondition: The result is `===` to `other`.
    ///
    /// - Complexity: O(1).
    public init(_ other: AnyForwardCollection<Element>)
    /// Create an `AnyForwardCollection` that stores `base` as its
    /// underlying collection.
    ///
    /// - Complexity: O(1).
    public init<C : CollectionType where C.Index : BidirectionalIndexType, C.Generator.Element == Element>(_ base: C)
    /// Create an `AnyForwardCollection` having the same underlying
    /// collection as `other`.
    ///
    /// - Postcondition: The result is `===` to `other`.
    ///
    /// - Complexity: O(1).
    public init(_ other: AnyBidirectionalCollection<Element>)
    /// Create an `AnyForwardCollection` that stores `base` as its
    /// underlying collection.
    ///
    /// - Complexity: O(1).
    public init<C : CollectionType where C.Index : RandomAccessIndexType, C.Generator.Element == Element>(_ base: C)
    /// Create an `AnyForwardCollection` having the same underlying
    /// collection as `other`.
    ///
    /// - Postcondition: The result is `===` to `other`.
    ///
    /// - Complexity: O(1).
    public init(_ other: AnyRandomAccessCollection<Element>)
    /// Returns a *generator* over the elements of this *collection*.
    ///
    /// - Complexity: O(1).
    public func generate() -> AnyGenerator<Element>
    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    public var startIndex: AnyForwardIndex { get }
    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    public var endIndex: AnyForwardIndex { get }
    public subscript (position: AnyForwardIndex) -> Element { get }
    /// Return the number of elements.
    ///
    /// - Complexity: O(N).
    public var count: IntMax { get }
}

extension AnyForwardCollection {
    public func underestimateCount() -> Int
}

/// A wrapper over an underlying `ForwardIndexType` that hides
/// the specific underlying type.
///
/// - SeeAlso: `AnyForwardCollection`
public struct AnyForwardIndex : ForwardIndexType {
    public typealias Distance = IntMax
    /// Wrap and forward operations to `base`.
    public init<BaseIndex : ForwardIndexType>(_ base: BaseIndex)
    /// Return the next consecutive value in a discrete sequence of
    /// `AnyForwardIndex` values.
    ///
    /// - Requires: `self` has a well-defined successor.
    public func successor() -> AnyForwardIndex
}

/// An abstract `GeneratorType` base class over `Element` elements.
///
/// Use this as a `Sequence`'s associated `Generator` type when you
/// don't want to expose details of the concrete generator, a subclass.
///
/// It is an error to create instances of `AnyGenerator` that are not
/// also instances of an `AnyGenerator` subclass.
///
/// - seealso:
///   - `struct AnySequence<S : SequenceType>`
///   - `func anyGenerator<G : GeneratorType>(base: G) -> AnyGenerator<G.Element>`
///   - `func anyGenerator<Element>(body: ()->Element?) -> AnyGenerator<Element>`
public class AnyGenerator<Element> : GeneratorType {
    /// Initialize the instance.  May only be called from a subclass
    /// initializer.
    public init()
    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Note: Subclasses must override this method.
    public func next() -> Element?
}

extension AnyGenerator : SequenceType {
}

/// The protocol to which all classes implicitly conform.
///
/// When used as a concrete type, all known `@objc` methods and
/// properties are available, as implicitly-unwrapped-optional methods
/// and properties respectively, on each instance of `AnyObject`.  For
/// example:
///
///     class C {
///       @objc func getCValue() -> Int { return 42 }
///     }
///
///     // If x has a method @objc getValue()->Int, call it and
///     // return the result.  Otherwise, return nil.
///     func getCValue1(x: AnyObject) -> Int? {
///       if let f: ()->Int = x.getCValue { // <===
///         return f()
///       }
///       return nil
///     }
///
///     // A more idiomatic implementation using "optional chaining"
///     func getCValue2(x: AnyObject) -> Int? {
///       return x.getCValue?() // <===
///     }
///
///     // An implementation that assumes the required method is present
///     func getCValue3(x: AnyObject) -> Int { // <===
///       return x.getCValue() // x.getCValue is implicitly unwrapped. // <===
///     }
///
/// - SeeAlso: `AnyClass`
@objc public protocol AnyObject {
}

/// A type-erased wrapper over any collection with indices that
/// support random access traversal.
///
/// Forwards operations to an arbitrary underlying collection having the
/// same `Element` type, hiding the specifics of the underlying
/// `CollectionType`.
///
/// - SeeAlso: `AnyForwardType`, `AnyBidirectionalType`
public struct AnyRandomAccessCollection<Element> : AnyCollectionType {
    /// Create an `AnyRandomAccessCollection` that stores `base` as its
    /// underlying collection.
    ///
    /// - Complexity: O(1).
    public init<C : CollectionType where C.Index : RandomAccessIndexType, C.Generator.Element == Element>(_ base: C)
    /// Create an `AnyRandomAccessCollection` having the same underlying
    /// collection as `other`.
    ///
    /// - Postcondition: The result is `===` to `other`.
    ///
    /// - Complexity: O(1).
    public init(_ other: AnyRandomAccessCollection<Element>)
    /// If the indices of the underlying collection stored by `other`
    /// satisfy `RandomAccessIndexType`, create an
    /// `AnyRandomAccessCollection` having the same underlying
    /// collection as `other`.  Otherwise, the result is `nil`.
    ///
    /// - Complexity: O(1).
    public init?(_ other: AnyForwardCollection<Element>)
    /// If the indices of the underlying collection stored by `other`
    /// satisfy `RandomAccessIndexType`, create an
    /// `AnyRandomAccessCollection` having the same underlying
    /// collection as `other`.  Otherwise, the result is `nil`.
    ///
    /// - Complexity: O(1).
    public init?(_ other: AnyBidirectionalCollection<Element>)
    /// Returns a *generator* over the elements of this *collection*.
    ///
    /// - Complexity: O(1).
    public func generate() -> AnyGenerator<Element>
    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    public var startIndex: AnyRandomAccessIndex { get }
    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    public var endIndex: AnyRandomAccessIndex { get }
    public subscript (position: AnyRandomAccessIndex) -> Element { get }
    /// Return the number of elements.
    ///
    /// - Complexity: O(1).
    public var count: IntMax { get }
}

extension AnyRandomAccessCollection {
    public func underestimateCount() -> Int
}

/// A wrapper over an underlying `RandomAccessIndexType` that hides
/// the specific underlying type.
///
/// - SeeAlso: `AnyRandomAccessCollection`
public struct AnyRandomAccessIndex : RandomAccessIndexType {
    public typealias Distance = IntMax
    /// Wrap and forward operations to `base`.
    public init<BaseIndex : RandomAccessIndexType>(_ base: BaseIndex)
    /// Return the next consecutive value in a discrete sequence of
    /// `AnyRandomAccessIndex` values.
    ///
    /// - Requires: `self` has a well-defined successor.
    public func successor() -> AnyRandomAccessIndex
    /// Return the previous consecutive value in a discrete sequence of
    /// `AnyRandomAccessIndex` values.
    ///
    /// - Requires: `self` has a well-defined predecessor.
    public func predecessor() -> AnyRandomAccessIndex
    public func distanceTo(other: AnyRandomAccessIndex) -> Distance
    public func advancedBy(amount: Distance) -> AnyRandomAccessIndex
    public func advancedBy(amount: Distance, limit: AnyRandomAccessIndex) -> AnyRandomAccessIndex
}

/// A type-erased sequence.
///
/// Forwards operations to an arbitrary underlying sequence having the
/// same `Element` type, hiding the specifics of the underlying
/// `SequenceType`.
///
/// - SeeAlso: `AnyGenerator<Element>`.
public struct AnySequence<Element> : SequenceType {
    /// Wrap and forward operations to to `base`.
    public init<S : SequenceType where S.Generator.Element == Element>(_ base: S)
    /// Create a sequence whose `generate()` method forwards to
    /// `makeUnderlyingGenerator`.
    public init<G : GeneratorType where G.Element == Element>(_ makeUnderlyingGenerator: () -> G)
    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    public func generate() -> AnyGenerator<Element>
}

extension AnySequence {
    public func underestimateCount() -> Int
}

/// `Array` is an efficient, tail-growable random-access
/// collection of arbitrary elements.
///
/// Common Properties of Array Types
/// ================================
///
/// The information in this section applies to all three of Swift's
/// array types, `Array<Element>`, `ContiguousArray<Element>`, and
/// `ArraySlice<Element>`.  When you read the word "array" here in
/// a normal typeface, it applies to all three of them.
///
/// Value Semantics
/// ---------------
///
/// Each array variable, `let` binding, or stored property has an
/// independent value that includes the values of all of its elements.
/// Therefore, mutations to the array are not observable through its
/// copies:
///
///     var a = [1, 2, 3]
///     var b = a
///     b[0] = 4
///     print("a=\(a), b=\(b)")     // a=[1, 2, 3], b=[4, 2, 3]
///
/// (Of course, if the array stores `class` references, the objects
/// are shared; only the values of the references are independent.)
///
/// Arrays use Copy-on-Write so that their storage and elements are
/// only copied lazily, upon mutation, when more than one array
/// instance is using the same buffer.  Therefore, the first in any
/// sequence of mutating operations may cost `O(N)` time and space,
/// where `N` is the length of the array.
///
/// Growth and Capacity
/// -------------------
///
/// When an array's contiguous storage fills up, new storage must be
/// allocated and elements must be moved to the new storage.  `Array`,
/// `ContiguousArray`, and `ArraySlice` share an exponential growth
/// strategy that makes `append` a constant time operation *when
/// amortized over many invocations*.  In addition to a `count`
/// property, these array types have a `capacity` that reflects their
/// potential to store elements without reallocation, and when you
/// know how many elements you'll store, you can call
/// `reserveCapacity` to pre-emptively reallocate and prevent
/// intermediate reallocations.
///
/// Objective-C Bridge
/// ==================
///
/// The main distinction between `Array` and the other array types is
/// that it interoperates seamlessly and efficiently with Objective-C.
///
/// `Array<Element>` is considered bridged to Objective-C iff `Element`
/// is bridged to Objective-C.
///
/// When `Element` is a `class` or `@objc` protocol type, `Array` may
/// store its elements in an `NSArray`.  Since any arbitrary subclass
/// of `NSArray` can become an `Array`, there are no guarantees about
/// representation or efficiency in this case (see also
/// `ContiguousArray`).  Since `NSArray` is immutable, it is just as
/// though the storage was shared by some copy: the first in any
/// sequence of mutating operations causes elements to be copied into
/// unique, contiguous storage which may cost `O(N)` time and space,
/// where `N` is the length of the array (or more, if the underlying
/// `NSArray` is has unusual performance characteristics).
///
/// Bridging to Objective-C
/// -----------------------
///
/// Any bridged `Array` can be implicitly converted to an `NSArray`.
/// When `Element` is a `class` or `@objc` protocol, bridging takes O(1)
/// time and O(1) space.  Other `Array`s must be bridged
/// element-by-element, allocating a new object for each element, at a
/// cost of at least O(`count`) time and space.
///
/// Bridging from Objective-C
/// -------------------------
///
/// An `NSArray` can be implicitly or explicitly converted to any
/// bridged `Array<Element>`.  This conversion calls `copyWithZone`
/// on the `NSArray`, to ensure it won't be modified, and stores the
/// result in the `Array`.  Type-checking, to ensure the `NSArray`'s
/// elements match or can be bridged to `Element`, is deferred until the
/// first element access.
public struct Array<Element> : CollectionType, MutableCollectionType, _DestructorSafeContainer {
    /// Always zero, which is the index of the first element when non-empty.
    public var startIndex: Int { get }
    /// A "past-the-end" element index; the successor of the last valid
    /// subscript argument.
    public var endIndex: Int { get }
    public subscript (index: Int) -> Element
    public subscript (subRange: Range<Int>) -> ArraySlice<Element>
}

extension Array : ArrayLiteralConvertible {
    /// Create an instance containing `elements`.
    public init(arrayLiteral elements: Element...)
}

extension Array : _ArrayType {
    /// Construct an empty Array.
    public init()
    /// Construct from an arbitrary sequence with elements of type `Element`.
    public init<S : SequenceType where S.Generator.Element == _Buffer.Element>(_ s: S)
    /// Construct a Array of `count` elements, each initialized to
    /// `repeatedValue`.
    public init(count: Int, repeatedValue: Element)
    /// The number of elements the Array stores.
    public var count: Int { get }
    /// The number of elements the `Array` can store without reallocation.
    public var capacity: Int { get }
    /// Reserve enough space to store `minimumCapacity` elements.
    ///
    /// - Postcondition: `capacity >= minimumCapacity` and the array has
    ///   mutable contiguous storage.
    ///
    /// - Complexity: O(`count`).
    public mutating func reserveCapacity(minimumCapacity: Int)
    /// Append `newElement` to the Array.
    ///
    /// - Complexity: Amortized O(1) unless `self`'s storage is shared with another live array; O(`count`) if `self` does not wrap a bridged `NSArray`; otherwise the efficiency is unspecified..
    public mutating func append(newElement: Element)
    /// Append the elements of `newElements` to `self`.
    ///
    /// - Complexity: O(*length of result*).
    public mutating func appendContentsOf<S : SequenceType where S.Generator.Element == Element>(newElements: S)
    /// Append the elements of `newElements` to `self`.
    ///
    /// - Complexity: O(*length of result*).
    public mutating func appendContentsOf<C : CollectionType where C.Generator.Element == Element>(newElements: C)
    /// Remove an element from the end of the Array in O(1).
    ///
    /// - Requires: `count > 0`.
    public mutating func removeLast() -> Element
    /// Insert `newElement` at index `i`.
    ///
    /// - Requires: `i <= count`.
    ///
    /// - Complexity: O(`count`).
    public mutating func insert(newElement: Element, atIndex i: Int)
    /// Remove and return the element at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`count`).
    public mutating func removeAtIndex(index: Int) -> Element
    /// Remove all elements.
    ///
    /// - Postcondition: `capacity == 0` iff `keepCapacity` is `false`.
    ///
    /// - Complexity: O(`self.count`).
    public mutating func removeAll(keepCapacity keepCapacity: Bool = default)
}

extension Array : _Reflectable {
}

extension Array : CustomStringConvertible, CustomDebugStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
}



extension Array {
    /// Call `body(p)`, where `p` is a pointer to the `Array`'s
    /// contiguous storage. If no such storage exists, it is first created.
    ///
    /// Often, the optimizer can eliminate bounds checks within an
    /// array algorithm, but when that fails, invoking the
    /// same algorithm on `body`'s argument lets you trade safety for
    /// speed.
    public func withUnsafeBufferPointer<R>(@noescape body: (UnsafeBufferPointer<Element>) throws -> R) rethrows -> R
    /// Call `body(p)`, where `p` is a pointer to the `Array`'s
    /// mutable contiguous storage. If no such storage exists, it is first created.
    ///
    /// Often, the optimizer can eliminate bounds- and uniqueness-checks
    /// within an array algorithm, but when that fails, invoking the
    /// same algorithm on `body`'s argument lets you trade safety for
    /// speed.
    ///
    /// - Warning: Do not rely on anything about `self` (the `Array`
    ///   that is the target of this method) during the execution of
    ///   `body`: it may not appear to have its correct value.  Instead,
    ///   use only the `UnsafeMutableBufferPointer` argument to `body`.
    public mutating func withUnsafeMutableBufferPointer<R>(@noescape body: (inout UnsafeMutableBufferPointer<Element>) throws -> R) rethrows -> R
}

extension Array {
    /// Replace the given `subRange` of elements with `newElements`.
    ///
    /// - Complexity: O(`subRange.count`) if `subRange.endIndex
    ///   == self.endIndex` and `newElements.isEmpty`, O(N) otherwise.
    public mutating func replaceRange<C : CollectionType where C.Generator.Element == _Buffer.Element>(subRange: Range<Int>, with newElements: C)
}



extension Array {
    /// If `!self.isEmpty`, remove the last element and return it, otherwise
    /// return `nil`.
    ///
    /// - Complexity: O(`self.count`) if the array is bridged,
    ///   otherwise O(1).
    public mutating func popLast() -> Element?
}

/// Conforming types can be initialized with array literals.
public protocol ArrayLiteralConvertible {
    typealias Element
    /// Create an instance initialized with `elements`.
    public init(arrayLiteral elements: Self.Element...)
}

/// The `Array`-like type that represents a sub-sequence of any
/// `Array`, `ContiguousArray`, or other `ArraySlice`.
///
/// `ArraySlice` always uses contiguous storage and does not bridge to
/// Objective-C.
///
/// - Warning: Long-term storage of `ArraySlice` instances is discouraged.
///
/// Because a `ArraySlice` presents a *view* onto the storage of some
/// larger array even after the original array's lifetime ends,
/// storing the slice may prolong the lifetime of elements that are
/// no longer accessible, which can manifest as apparent memory and
/// object leakage.  To prevent this effect, use `ArraySlice` only for
/// transient computation.
public struct ArraySlice<Element> : CollectionType, MutableCollectionType, _DestructorSafeContainer {
    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    public var startIndex: Int { get }
    /// A "past-the-end" element index; the successor of the last valid
    /// subscript argument.
    public var endIndex: Int { get }
    public subscript (index: Int) -> Element
    public subscript (subRange: Range<Int>) -> ArraySlice<Element>
}

extension ArraySlice : ArrayLiteralConvertible {
    /// Create an instance containing `elements`.
    public init(arrayLiteral elements: Element...)
}

extension ArraySlice : _ArrayType {
    /// Construct an empty ArraySlice.
    public init()
    /// Construct from an arbitrary sequence with elements of type `Element`.
    public init<S : SequenceType where S.Generator.Element == _Buffer.Element>(_ s: S)
    /// Construct a ArraySlice of `count` elements, each initialized to
    /// `repeatedValue`.
    public init(count: Int, repeatedValue: Element)
    /// The number of elements the ArraySlice stores.
    public var count: Int { get }
    /// The number of elements the `ArraySlice` can store without reallocation.
    public var capacity: Int { get }
    /// Reserve enough space to store `minimumCapacity` elements.
    ///
    /// - Postcondition: `capacity >= minimumCapacity` and the array has
    ///   mutable contiguous storage.
    ///
    /// - Complexity: O(`count`).
    public mutating func reserveCapacity(minimumCapacity: Int)
    /// Append `newElement` to the ArraySlice.
    ///
    /// - Complexity: Amortized O(1) unless `self`'s storage is shared with another live array; O(`count`) otherwise..
    public mutating func append(newElement: Element)
    /// Append the elements of `newElements` to `self`.
    ///
    /// - Complexity: O(*length of result*).
    public mutating func appendContentsOf<S : SequenceType where S.Generator.Element == Element>(newElements: S)
    /// Append the elements of `newElements` to `self`.
    ///
    /// - Complexity: O(*length of result*).
    public mutating func appendContentsOf<C : CollectionType where C.Generator.Element == Element>(newElements: C)
    /// Remove an element from the end of the ArraySlice in O(1).
    ///
    /// - Requires: `count > 0`.
    public mutating func removeLast() -> Element
    /// Insert `newElement` at index `i`.
    ///
    /// - Requires: `i <= count`.
    ///
    /// - Complexity: O(`count`).
    public mutating func insert(newElement: Element, atIndex i: Int)
    /// Remove and return the element at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`count`).
    public mutating func removeAtIndex(index: Int) -> Element
    /// Remove all elements.
    ///
    /// - Postcondition: `capacity == 0` iff `keepCapacity` is `false`.
    ///
    /// - Complexity: O(`self.count`).
    public mutating func removeAll(keepCapacity keepCapacity: Bool = default)
}

extension ArraySlice : _Reflectable {
}

extension ArraySlice : CustomStringConvertible, CustomDebugStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
}



extension ArraySlice {
    /// Call `body(p)`, where `p` is a pointer to the `ArraySlice`'s
    /// contiguous storage.
    ///
    /// Often, the optimizer can eliminate bounds checks within an
    /// array algorithm, but when that fails, invoking the
    /// same algorithm on `body`'s argument lets you trade safety for
    /// speed.
    public func withUnsafeBufferPointer<R>(@noescape body: (UnsafeBufferPointer<Element>) throws -> R) rethrows -> R
    /// Call `body(p)`, where `p` is a pointer to the `ArraySlice`'s
    /// mutable contiguous storage.
    ///
    /// Often, the optimizer can eliminate bounds- and uniqueness-checks
    /// within an array algorithm, but when that fails, invoking the
    /// same algorithm on `body`'s argument lets you trade safety for
    /// speed.
    ///
    /// - Warning: Do not rely on anything about `self` (the `ArraySlice`
    ///   that is the target of this method) during the execution of
    ///   `body`: it may not appear to have its correct value.  Instead,
    ///   use only the `UnsafeMutableBufferPointer` argument to `body`.
    public mutating func withUnsafeMutableBufferPointer<R>(@noescape body: (inout UnsafeMutableBufferPointer<Element>) throws -> R) rethrows -> R
}

extension ArraySlice {
    /// Replace the given `subRange` of elements with `newElements`.
    ///
    /// - Complexity: O(`subRange.count`) if `subRange.endIndex
    ///   == self.endIndex` and `newElements.isEmpty`, O(N) otherwise.
    public mutating func replaceRange<C : CollectionType where C.Generator.Element == _Buffer.Element>(subRange: Range<Int>, with newElements: C)
}

/// A mutable pointer-to-ObjC-pointer argument.
///
/// This type has implicit conversions to allow passing any of the following
/// to a C or ObjC API:
///
/// - `nil`, which gets passed as a null pointer,
/// - an inout argument of the referenced type, which gets passed as a pointer
///   to a writeback temporary with autoreleasing ownership semantics,
/// - an `UnsafeMutablePointer<Memory>`, which is passed as-is.
///
/// Passing pointers to mutable arrays of ObjC class pointers is not
/// directly supported. Unlike `UnsafeMutablePointer<Memory>`,
/// `AutoreleasingUnsafeMutablePointer<Memory>` must reference storage that
/// does not own a reference count to the referenced
/// value. UnsafeMutablePointer's operations, by contrast, assume that
/// the referenced storage owns values loaded from or stored to it.
///
/// This type does not carry an owner pointer unlike the other C*Pointer types
/// because it only needs to reference the results of inout conversions, which
/// already have writeback-scoped lifetime.
public struct AutoreleasingUnsafeMutablePointer<Memory> : Equatable, NilLiteralConvertible, _PointerType {
    /// Access the underlying raw memory, getting and
    /// setting values.
    public var memory: Memory { get nonmutating set }
    public subscript (i: Int) -> Memory { get }
    /// Create an instance initialized with `nil`.
    public init(nilLiteral: ())
    /// Initialize to a null pointer.
    public init()
    /// Explicit construction from an UnsafeMutablePointer.
    ///
    /// This is inherently unsafe; UnsafeMutablePointer assumes the
    /// referenced memory has +1 strong ownership semantics, whereas
    /// AutoreleasingUnsafeMutablePointer implies +0 semantics.
    public init<U>(_ ptr: UnsafeMutablePointer<U>)
}

extension AutoreleasingUnsafeMutablePointer : CustomDebugStringConvertible {
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
}

extension AutoreleasingUnsafeMutablePointer : CVarArgType {
}

/// An *index* that can step backwards via application of its
/// `predecessor()` method.
public protocol BidirectionalIndexType : ForwardIndexType {
    /// Return the previous consecutive value in a discrete sequence.
    ///
    /// If `self` has a well-defined successor,
    /// `self.successor().predecessor() == self`.  If `self` has a
    /// well-defined predecessor, `self.predecessor().successor() ==
    /// self`.
    ///
    /// - Requires: `self` has a well-defined predecessor.
    @warn_unused_result
    public func predecessor() -> Self
}

extension BidirectionalIndexType {
    @warn_unused_result
    public func advancedBy(n: Self.Distance) -> Self
    @warn_unused_result
    public func advancedBy(n: Self.Distance, limit: Self) -> Self
}

extension BidirectionalIndexType where Self : ReverseIndexType {
    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    public func successor() -> Self
    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    public func predecessor() -> Self
}

/// A `RandomAccessIndexType` that has two possible values.  Used as
/// the `Index` type for `CollectionOfOne<T>`.
public enum Bit : Int, Comparable, RandomAccessIndexType, _Reflectable {
    public typealias Distance = Int
    case Zero
    case One
    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: `self == .Zero`.
    public func successor() -> Bit
    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: `self != .Zero`.
    public func predecessor() -> Bit
    public func distanceTo(other: Bit) -> Int
    public func advancedBy(n: Distance) -> Bit
}

extension Bit : IntegerArithmeticType {
    /// Add `lhs` and `rhs`, returning a result and a `Bool` that is
    /// true iff the operation caused an arithmetic overflow.
    public static func addWithOverflow(lhs: Bit, _ rhs: Bit) -> (Bit, overflow: Bool)
    /// Subtract `lhs` and `rhs`, returning a result and a `Bool` that is
    /// true iff the operation caused an arithmetic overflow.
    public static func subtractWithOverflow(lhs: Bit, _ rhs: Bit) -> (Bit, overflow: Bool)
    /// Multiply `lhs` and `rhs`, returning a result and a `Bool` that is
    /// true iff the operation caused an arithmetic overflow.
    public static func multiplyWithOverflow(lhs: Bit, _ rhs: Bit) -> (Bit, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning a result and a `Bool` that is
    /// true iff the operation caused an arithmetic overflow.
    public static func divideWithOverflow(lhs: Bit, _ rhs: Bit) -> (Bit, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning the remainder and a `Bool` that is
    /// true iff the operation caused an arithmetic overflow.
    public static func remainderWithOverflow(lhs: Bit, _ rhs: Bit) -> (Bit, overflow: Bool)
    /// Represent this number using Swift's widest native signed integer
    /// type.
    public func toIntMax() -> IntMax
}

/// A set type with O(1) standard bitwise operators.
///
/// Each instance is a subset of `~Self.allZeros`.
///
/// **Axioms**, where `x` is an instance of `Self`:
///
/// -  `x | Self.allZeros == x`
/// -  `x ^ Self.allZeros == x`
/// -  `x & Self.allZeros == .allZeros`
/// -  `x & ~Self.allZeros == x`
/// -  `~x == x ^ ~Self.allZeros`
public protocol BitwiseOperationsType {
    /// Returns the intersection of bits set in `lhs` and `rhs`.
    ///
    /// - Complexity: O(1).
    @warn_unused_result
    public func &(lhs: Self, rhs: Self) -> Self
    /// Returns the union of bits set in `lhs` and `rhs`.
    ///
    /// - Complexity: O(1).
    @warn_unused_result
    public func |(lhs: Self, rhs: Self) -> Self
    /// Returns the bits that are set in exactly one of `lhs` and `rhs`.
    ///
    /// - Complexity: O(1).
    @warn_unused_result
    public func ^(lhs: Self, rhs: Self) -> Self
    /// Returns `x ^ ~Self.allZeros`.
    ///
    /// - Complexity: O(1).
    @warn_unused_result
    prefix public func ~(x: Self) -> Self
    /// The empty bitset.
    ///
    /// Also the [identity element](http://en.wikipedia.org/wiki/Identity_element) for `|` and
    /// `^`, and the [fixed point](http://en.wikipedia.org/wiki/Fixed_point_(mathematics)) for
    /// `&`.
    public static var allZeros: Self { get }
}

/// A value type whose instances are either `true` or `false`.
public struct Bool {
    /// Default-initialize Boolean value to `false`.
    public init()
}

extension Bool : BooleanLiteralConvertible {
    public init(_builtinBooleanLiteral value: Builtin.Int1)
    /// Create an instance initialized to `value`.
    public init(booleanLiteral value: Bool)
}

extension Bool : BooleanType {
    /// Identical to `self`.
    public var boolValue: Bool { get }
    /// Construct an instance representing the same logical value as
    /// `value`.
    public init<T : BooleanType>(_ value: T)
}

extension Bool : CustomStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
}

extension Bool : Equatable, Hashable {
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: the hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
}

extension Bool : _Reflectable {
}

/// Conforming types can be initialized with the Boolean literals
/// `true` and `false`.
public protocol BooleanLiteralConvertible {
    typealias BooleanLiteralType
    /// Create an instance initialized to `value`.
    public init(booleanLiteral value: Self.BooleanLiteralType)
}

/// The default type for an otherwise-unconstrained Boolean literal.
public typealias BooleanLiteralType = Bool

/// A type that represents a Boolean value.
///
/// Types that conform to the `BooleanType` protocol can be used as
/// the condition in control statements (`if`, `while`, C-style `for`)
/// and other logical value contexts (e.g., `case` statement guards).
///
/// Only three types provided by Swift, `Bool`, `DarwinBoolean`, and `ObjCBool`,
/// conform to `BooleanType`. Expanding this set to include types that
/// represent more than simple boolean values is discouraged.
public protocol BooleanType {
    /// The value of `self`, expressed as a `Bool`.
    public var boolValue: Bool { get }
}

/// The C '_Bool' and C++ 'bool' type.
public typealias CBool = Bool

/// The C 'char' type.
///
/// This will be the same as either `CSignedChar` (in the common
/// case) or `CUnsignedChar`, depending on the platform.
public typealias CChar = Int8

/// The C++11 'char16_t' type, which has UTF-16 encoding.
public typealias CChar16 = UInt16

/// The C++11 'char32_t' type, which has UTF-32 encoding.
public typealias CChar32 = UnicodeScalar

/// The C 'double' type.
public typealias CDouble = Double

/// The C 'float' type.
public typealias CFloat = Float

/// The C 'int' type.
public typealias CInt = Int32

/// The C 'long' type.
public typealias CLong = Int

/// The C 'long long' type.
public typealias CLongLong = Int64

/// A wrapper around an opaque C pointer.
///
/// Opaque pointers are used to represent C pointers to types that
/// cannot be represented in Swift, such as incomplete struct types.
public struct COpaquePointer : Equatable, Hashable, NilLiteralConvertible {
    /// Construct a `nil` instance.
    public init()
    /// Construct a `COpaquePointer` from a given address in memory.
    ///
    /// This is a fundamentally unsafe conversion.
    public init(bitPattern: Int)
    /// Construct a `COpaquePointer` from a given address in memory.
    ///
    /// This is a fundamentally unsafe conversion.
    public init(bitPattern: UInt)
    /// Convert a typed `UnsafePointer` to an opaque C pointer.
    public init<T>(_ source: UnsafePointer<T>)
    /// Convert a typed `UnsafeMutablePointer` to an opaque C pointer.
    public init<T>(_ source: UnsafeMutablePointer<T>)
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
    /// Create an instance initialized with `nil`.
    public init(nilLiteral: ())
}

extension COpaquePointer : CustomDebugStringConvertible {
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
}

extension COpaquePointer : CVarArgType {
}

/// The C 'short' type.
public typealias CShort = Int16

/// The C 'signed char' type.
public typealias CSignedChar = Int8

/// The C 'unsigned char' type.
public typealias CUnsignedChar = UInt8

/// The C 'unsigned int' type.
public typealias CUnsignedInt = UInt32

/// The C 'unsigned long' type.
public typealias CUnsignedLong = UInt

/// The C 'unsigned long long' type.
public typealias CUnsignedLongLong = UInt64

/// The C 'unsigned short' type.
public typealias CUnsignedShort = UInt16

/// The corresponding Swift type to `va_list` in imported C APIs.
public struct CVaListPointer {
    public init(_fromUnsafeMutablePointer from: UnsafeMutablePointer<Void>)
}

extension CVaListPointer : CustomDebugStringConvertible {
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
}

/// Instances of conforming types can be encoded, and appropriately
/// passed, as elements of a C `va_list`.
///
/// This protocol is useful in presenting C "varargs" APIs natively in
/// Swift.  It only works for APIs that have a `va_list` variant, so
/// for example, it isn't much use if all you have is:
///
///     int f(int n, ...)
///
/// Given a version like this, though,
///
///     int f(int, va_list arguments)
///
/// you can write:
///
///     func swiftF(x: Int, arguments: CVarArgType...) -> Int {
///       return withVaList(arguments) { f(x, $0) }
///     }
public protocol CVarArgType {
}

/// The C++ 'wchar_t' type.
public typealias CWideChar = UnicodeScalar

/// `Character` represents some Unicode grapheme cluster as
/// defined by a canonical, localized, or otherwise tailored
/// segmentation algorithm.
public struct Character : ExtendedGraphemeClusterLiteralConvertible, Equatable, Hashable, Comparable {
    /// Construct a `Character` containing just the given `scalar`.
    public init(_ scalar: UnicodeScalar)
    public init(_builtinUnicodeScalarLiteral value: Builtin.Int32)
    /// Create an instance initialized to `value`.
    public init(unicodeScalarLiteral value: Character)
    public init(_builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer, byteSize: Builtin.Word, isASCII: Builtin.Int1)
    /// Create an instance initialized to `value`.
    public init(extendedGraphemeClusterLiteral value: Character)
    /// Create an instance from a single-character `String`.
    ///
    /// - Requires: `s` contains exactly one extended grapheme cluster.
    public init(_ s: String)
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
}

extension Character : CustomDebugStringConvertible {
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
}

extension Character : _Reflectable {
}

extension Character : Streamable {
    /// Write a textual representation of `self` into `target`.
    public func writeTo<Target : OutputStreamType>(inout target: Target)
}

/// A closed `IntervalType`, which contains both its `start` and its
/// `end`.  Cannot represent an empty interval.
///
/// - parameter Bound: The type of the endpoints.
public struct ClosedInterval<Bound : Comparable> : IntervalType, Equatable, CustomStringConvertible, CustomDebugStringConvertible, _Reflectable {
    /// Construct a copy of `x`.
    public init(_ x: ClosedInterval<Bound>)
    /// Construct an interval with the given bounds.
    ///
    /// - Requires: `start <= end`.
    public init(_ start: Bound, _ end: Bound)
    /// The `Interval`'s lower bound.
    ///
    /// Invariant: `start` <= `end`.
    public var start: Bound { get }
    /// The `Interval`'s upper bound.
    ///
    /// Invariant: `start` <= `end`.
    public var end: Bound { get }
    /// A textual representation of `self`.
    public var description: String { get }
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
    /// Returns `true` iff the `Interval` contains `x`.
    @warn_unused_result
    public func contains(x: Bound) -> Bool
    /// Returns `intervalToClamp` clamped to `self`.
    ///
    /// The bounds of the result, even if it is empty, are always limited to the bounds of
    /// `self`.
    @warn_unused_result
    public func clamp(intervalToClamp: ClosedInterval<Bound>) -> ClosedInterval<Bound>
}

extension ClosedInterval {
    /// `true` iff the `Interval` is empty.  In the case of
    /// `ClosedInterval`, always returns `false`.
    public var isEmpty: Bool { get }
}

/// A collection containing a single element of type `Element`.
public struct CollectionOfOne<Element> : CollectionType {
    /// A type that represents a valid position in the collection.
    ///
    /// Valid indices consist of the position of every element and a
    /// "past the end" position that's not valid for use as a subscript.
    public typealias Index = Bit
    /// Construct an instance containing just `element`.
    public init(_ element: Element)
    /// The position of the first element.
    public var startIndex: Index { get }
    /// The "past the end" position; always identical to
    /// `startIndex.successor()`.
    ///
    /// - Note: `endIndex` is not a valid argument to `subscript`.
    public var endIndex: Index { get }
    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    public func generate() -> GeneratorOfOne<Element>
    public subscript (position: Index) -> Element { get }
    /// Return the number of elements (always one).
    public var count: Int { get }
}

extension CollectionOfOne : _Reflectable {
}

/// A multi-pass *sequence* with addressable positions.
///
/// Positions are represented by an associated `Index` type.  Whereas
/// an arbitrary *sequence* may be consumed as it is traversed, a
/// *collection* is multi-pass: any element may be revisited merely by
/// saving its index.
///
/// The sequence view of the elements is identical to the collection
/// view.  In other words, the following code binds the same series of
/// values to `x` as does `for x in self {}`:
///
///     for i in startIndex..<endIndex {
///       let x = self[i]
///     }
public protocol CollectionType : Indexable, SequenceType {
    /// A type that provides the *sequence*'s iteration interface and
    /// encapsulates its iteration state.
    ///
    /// By default, a `CollectionType` satisfies `SequenceType` by
    /// supplying an `IndexingGenerator` as its associated `Generator`
    /// type.
    typealias Generator : GeneratorType = IndexingGenerator<Self>
    public func generate() -> Self.Generator
    /// A `SequenceType` that can represent a contiguous subrange of `self`'s
    /// elements.
    ///
    /// - Note: this associated type appears as a requirement in
    ///   `SequenceType`, but is restated here with stricter
    ///   constraints: in a `CollectionType`, the `SubSequence` should
    ///   also be a `CollectionType`.
    typealias SubSequence : Indexable, SequenceType = Slice<Self>
    public subscript (position: Self.Index) -> Self.Generator.Element { get }
    public subscript (bounds: Range<Self.Index>) -> Self.SubSequence { get }
    /// Returns `self[startIndex..<end]`
    ///
    /// - Complexity: O(1)
    @warn_unused_result
    public func prefixUpTo(end: Self.Index) -> Self.SubSequence
    /// Returns `self[start..<endIndex]`
    ///
    /// - Complexity: O(1)
    @warn_unused_result
    public func suffixFrom(start: Self.Index) -> Self.SubSequence
    /// Returns `prefixUpTo(position.successor())`
    ///
    /// - Complexity: O(1)
    @warn_unused_result
    public func prefixThrough(position: Self.Index) -> Self.SubSequence
    /// Returns `true` iff `self` is empty.
    public var isEmpty: Bool { get }
    /// Returns the number of elements.
    ///
    /// - Complexity: O(1) if `Index` conforms to `RandomAccessIndexType`;
    ///   O(N) otherwise.
    public var count: Self.Index.Distance { get }
    /// Returns the first element of `self`, or `nil` if `self` is empty.
    public var first: Self.Generator.Element? { get }
}

extension CollectionType where Generator == IndexingGenerator<Self> {
    public func generate() -> IndexingGenerator<Self>
}

extension CollectionType where SubSequence == Slice<Self> {
    public subscript (bounds: Range<Self.Index>) -> Slice<Self> { get }
}

extension CollectionType where SubSequence == Self {
    /// If `!self.isEmpty`, remove the first element and return it, otherwise
    /// return `nil`.
    ///
    /// - Complexity: O(`self.count`)
    @warn_unused_result
    public mutating func popFirst() -> Self.Generator.Element?
    /// If `!self.isEmpty`, remove the last element and return it, otherwise
    /// return `nil`.
    ///
    /// - Complexity: O(`self.count`)
    @warn_unused_result
    public mutating func popLast() -> Self.Generator.Element?
}

extension CollectionType {
    /// Returns `true` iff `self` is empty.
    ///
    /// - Complexity: O(1)
    public var isEmpty: Bool { get }
    /// Returns the first element of `self`, or `nil` if `self` is empty.
    ///
    /// - Complexity: O(1)
    public var first: Self.Generator.Element? { get }
    /// Returns a value less than or equal to the number of elements in
    /// `self`, *nondestructively*.
    ///
    /// - Complexity: O(N).
    public func underestimateCount() -> Int
    /// Returns the number of elements.
    ///
    /// - Complexity: O(1) if `Index` conforms to `RandomAccessIndexType`;
    ///   O(N) otherwise.
    public var count: Self.Index.Distance { get }
}

extension CollectionType {
    /// Return an `Array` containing the results of mapping `transform`
    /// over `self`.
    ///
    /// - Complexity: O(N).
    @warn_unused_result
    public func map<T>(@noescape transform: (Self.Generator.Element) throws -> T) rethrows -> [T]
    /// Returns a subsequence containing all but the first `n` elements.
    ///
    /// - Requires: `n >= 0`
    /// - Complexity: O(`n`)
    @warn_unused_result
    public func dropFirst(n: Int) -> Self.SubSequence
    /// Returns a subsequence containing all but the last `n` elements.
    ///
    /// - Requires: `n >= 0`
    /// - Complexity: O(`self.count`)
    @warn_unused_result
    public func dropLast(n: Int) -> Self.SubSequence
    /// Returns a subsequence, up to `maxLength` in length, containing the
    /// initial elements.
    ///
    /// If `maxLength` exceeds `self.count`, the result contains all
    /// the elements of `self`.
    ///
    /// - Requires: `maxLength >= 0`
    /// - Complexity: O(`maxLength`)
    @warn_unused_result
    public func prefix(maxLength: Int) -> Self.SubSequence
    /// Returns a slice, up to `maxLength` in length, containing the
    /// final elements of `s`.
    ///
    /// If `maxLength` exceeds `s.count`, the result contains all
    /// the elements of `s`.
    ///
    /// - Requires: `maxLength >= 0`
    /// - Complexity: O(`self.count`)
    @warn_unused_result
    public func suffix(maxLength: Int) -> Self.SubSequence
    /// Returns `self[startIndex..<end]`
    ///
    /// - Complexity: O(1)
    @warn_unused_result
    public func prefixUpTo(end: Self.Index) -> Self.SubSequence
    /// Returns `self[start..<endIndex]`
    ///
    /// - Complexity: O(1)
    @warn_unused_result
    public func suffixFrom(start: Self.Index) -> Self.SubSequence
    /// Returns `prefixUpTo(position.successor())`
    ///
    /// - Complexity: O(1)
    @warn_unused_result
    public func prefixThrough(position: Self.Index) -> Self.SubSequence
    /// Returns the maximal `SubSequence`s of `self`, in order, that
    /// don't contain elements satisfying the predicate `isSeparator`.
    ///
    /// - Parameter maxSplit: The maximum number of `SubSequence`s to
    ///   return, minus 1.
    ///   If `maxSplit + 1` `SubSequence`s are returned, the last one is
    ///   a suffix of `self` containing the remaining elements.
    ///   The default value is `Int.max`.
    ///
    /// - Parameter allowEmptySubsequences: If `true`, an empty `SubSequence`
    ///   is produced in the result for each pair of consecutive elements
    ///   satisfying `isSeparator`.
    ///   The default value is `false`.
    ///
    /// - Requires: `maxSplit >= 0`
    @warn_unused_result
    public func split(maxSplit: Int = default, allowEmptySlices: Bool = default, @noescape isSeparator: (Self.Generator.Element) throws -> Bool) rethrows -> [Self.SubSequence]
}

extension CollectionType where Generator.Element : Equatable {
    /// Returns the maximal `SubSequence`s of `self`, in order, around a
    /// `separator` element.
    ///
    /// - Parameter maxSplit: The maximum number of `SubSequence`s to
    ///   return, minus 1.
    ///   If `maxSplit + 1` `SubSequence`s are returned, the last one is
    ///   a suffix of `self` containing the remaining elements.
    ///   The default value is `Int.max`.
    ///
    /// - Parameter allowEmptySubsequences: If `true`, an empty `SubSequence`
    ///   is produced in the result for each pair of consecutive elements
    ///   satisfying `isSeparator`.
    ///   The default value is `false`.
    ///
    /// - Requires: `maxSplit >= 0`
    @warn_unused_result
    public func split(separator: Self.Generator.Element, maxSplit: Int = default, allowEmptySlices: Bool = default) -> [Self.SubSequence]
}

extension CollectionType where Index : BidirectionalIndexType {
    /// Returns a subsequence containing all but the last `n` elements.
    ///
    /// - Requires: `n >= 0`
    /// - Complexity: O(`n`)
    @warn_unused_result
    public func dropLast(n: Int) -> Self.SubSequence
    /// Returns a slice, up to `maxLength` in length, containing the
    /// final elements of `s`.
    ///
    /// If `maxLength` exceeds `s.count`, the result contains all
    /// the elements of `s`.
    ///
    /// - Requires: `maxLength >= 0`
    /// - Complexity: O(`maxLength`)
    @warn_unused_result
    public func suffix(maxLength: Int) -> Self.SubSequence
}

extension CollectionType where SubSequence == Self {
    public mutating func removeFirst() -> Self.Generator.Element
}



extension CollectionType where Index : BidirectionalIndexType {
    public var last: Self.Generator.Element? { get }
}

extension CollectionType where Generator.Element : Equatable {
    /// Returns the first index where `value` appears in `self` or `nil` if
    /// `value` is not found.
    ///
    /// - Complexity: O(`self.count`).
    @warn_unused_result
    public func indexOf(element: Self.Generator.Element) -> Self.Index?
}

extension CollectionType {
    /// Returns the first index where `predicate` returns `true` for the
    /// corresponding value, or `nil` if such value is not found.
    ///
    /// - Complexity: O(`self.count`).
    @warn_unused_result
    public func indexOf(@noescape predicate: (Self.Generator.Element) throws -> Bool) rethrows -> Self.Index?
}

extension CollectionType {
    /// Return the range of valid index values.
    ///
    /// The result's `endIndex` is the same as that of `self`.  Because
    /// `Range` is half-open, iterating the values of the result produces
    /// all valid subscript arguments for `self`, omitting its `endIndex`.
    public var indices: Range<Self.Index> { get }
}



extension CollectionType where Generator.Element : CollectionType {
    /// A concatenation of the elements of `self`.
    @warn_unused_result
    public func flatten() -> FlattenCollection<Self>
}

extension CollectionType where Generator.Element : CollectionType, Index : BidirectionalIndexType, Generator.Element.Index : BidirectionalIndexType {
    /// A concatenation of the elements of `self`.
    @warn_unused_result
    public func flatten() -> FlattenBidirectionalCollection<Self>
}

extension CollectionType {
    /// A collection with contents identical to `self`, but on which
    /// normally-eager operations such as `map` and `filter` are
    /// implemented lazily.
    ///
    /// - See Also: `LazySequenceType`, `LazyCollectionType`.
    public var lazy: LazyCollection<Self> { get }
}

extension CollectionType where Self : _ReverseCollectionType, Self.Base.Index : RandomAccessIndexType {
    public var startIndex: ReverseRandomAccessIndex<Self.Base.Index> { get }
}

extension CollectionType where Index : BidirectionalIndexType {
    /// Return the elements of `self` in reverse order.
    ///
    /// - Complexity: O(1)
    @warn_unused_result
    public func reverse() -> ReverseCollection<Self>
}

extension CollectionType where Index : RandomAccessIndexType {
    /// Return the elements of `self` in reverse order.
    ///
    /// - Complexity: O(1)
    @warn_unused_result
    public func reverse() -> ReverseRandomAccessCollection<Self>
}

extension CollectionType where Self : _CollectionWrapperType, Self.Index == Self.Base.Index {
    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    public var startIndex: Self.Index { get }
    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    public var endIndex: Self.Index { get }
    public subscript (position: Self.Index) -> Self.Base.Generator.Element { get }
    @warn_unused_result
    public func map<T>(@noescape transform: (Self.Base.Generator.Element) -> T) -> [T]
    @warn_unused_result
    public func filter(@noescape includeElement: (Self.Base.Generator.Element) -> Bool) -> [Self.Base.Generator.Element]
}

/// Instances of conforming types can be compared using relational
/// operators, which define a [strict total order](http://en.wikipedia.org/wiki/Total_order#Strict_total_order).
///
/// A type conforming to `Comparable` need only supply the `<` and
/// `==` operators; default implementations of `<=`, `>`, `>=`, and
/// `!=` are supplied by the standard library:
///
///     struct Singular : Comparable {}
///     func ==(x: Singular, y: Singular) -> Bool { return true }
///     func <(x: Singular, y: Singular) -> Bool { return false }
///
/// **Axioms**, in addition to those of `Equatable`:
///
/// - `x == y` implies `x <= y`, `x >= y`, `!(x < y)`, and `!(x > y)`
/// - `x < y` implies `x <= y` and `y > x`
/// - `x > y` implies `x >= y` and `y < x`
/// - `x <= y` implies `y >= x`
/// - `x >= y` implies `y <= x`
public protocol Comparable : Equatable {
    /// A [strict total order](http://en.wikipedia.org/wiki/Total_order#Strict_total_order)
    /// over instances of `Self`.
    @warn_unused_result
    public func <(lhs: Self, rhs: Self) -> Bool
    @warn_unused_result
    public func <=(lhs: Self, rhs: Self) -> Bool
    @warn_unused_result
    public func >=(lhs: Self, rhs: Self) -> Bool
    @warn_unused_result
    public func >(lhs: Self, rhs: Self) -> Bool
}

/// A fast, contiguously-stored array of `Element`.
///
/// Efficiency is equivalent to that of `Array`, unless `Element` is a
/// `class` or `@objc` `protocol` type, in which case using
/// `ContiguousArray` may be more efficient.  Note, however, that
/// `ContiguousArray` does not bridge to Objective-C.  See `Array`,
/// with which `ContiguousArray` shares most properties, for more
/// detail.
public struct ContiguousArray<Element> : CollectionType, MutableCollectionType, _DestructorSafeContainer {
    /// Always zero, which is the index of the first element when non-empty.
    public var startIndex: Int { get }
    /// A "past-the-end" element index; the successor of the last valid
    /// subscript argument.
    public var endIndex: Int { get }
    public subscript (index: Int) -> Element
    public subscript (subRange: Range<Int>) -> ArraySlice<Element>
}

extension ContiguousArray : ArrayLiteralConvertible {
    /// Create an instance containing `elements`.
    public init(arrayLiteral elements: Element...)
}

extension ContiguousArray : _ArrayType {
    /// Construct an empty ContiguousArray.
    public init()
    /// Construct from an arbitrary sequence with elements of type `Element`.
    public init<S : SequenceType where S.Generator.Element == _Buffer.Element>(_ s: S)
    /// Construct a ContiguousArray of `count` elements, each initialized to
    /// `repeatedValue`.
    public init(count: Int, repeatedValue: Element)
    /// The number of elements the ContiguousArray stores.
    public var count: Int { get }
    /// The number of elements the `ContiguousArray` can store without reallocation.
    public var capacity: Int { get }
    /// Reserve enough space to store `minimumCapacity` elements.
    ///
    /// - Postcondition: `capacity >= minimumCapacity` and the array has
    ///   mutable contiguous storage.
    ///
    /// - Complexity: O(`count`).
    public mutating func reserveCapacity(minimumCapacity: Int)
    /// Append `newElement` to the ContiguousArray.
    ///
    /// - Complexity: Amortized O(1) unless `self`'s storage is shared with another live array; O(`count`) otherwise..
    public mutating func append(newElement: Element)
    /// Append the elements of `newElements` to `self`.
    ///
    /// - Complexity: O(*length of result*).
    public mutating func appendContentsOf<S : SequenceType where S.Generator.Element == Element>(newElements: S)
    /// Append the elements of `newElements` to `self`.
    ///
    /// - Complexity: O(*length of result*).
    public mutating func appendContentsOf<C : CollectionType where C.Generator.Element == Element>(newElements: C)
    /// Remove an element from the end of the ContiguousArray in O(1).
    ///
    /// - Requires: `count > 0`.
    public mutating func removeLast() -> Element
    /// Insert `newElement` at index `i`.
    ///
    /// - Requires: `i <= count`.
    ///
    /// - Complexity: O(`count`).
    public mutating func insert(newElement: Element, atIndex i: Int)
    /// Remove and return the element at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`count`).
    public mutating func removeAtIndex(index: Int) -> Element
    /// Remove all elements.
    ///
    /// - Postcondition: `capacity == 0` iff `keepCapacity` is `false`.
    ///
    /// - Complexity: O(`self.count`).
    public mutating func removeAll(keepCapacity keepCapacity: Bool = default)
}

extension ContiguousArray : _Reflectable {
}

extension ContiguousArray : CustomStringConvertible, CustomDebugStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
}



extension ContiguousArray {
    /// Call `body(p)`, where `p` is a pointer to the `ContiguousArray`'s
    /// contiguous storage.
    ///
    /// Often, the optimizer can eliminate bounds checks within an
    /// array algorithm, but when that fails, invoking the
    /// same algorithm on `body`'s argument lets you trade safety for
    /// speed.
    public func withUnsafeBufferPointer<R>(@noescape body: (UnsafeBufferPointer<Element>) throws -> R) rethrows -> R
    /// Call `body(p)`, where `p` is a pointer to the `ContiguousArray`'s
    /// mutable contiguous storage.
    ///
    /// Often, the optimizer can eliminate bounds- and uniqueness-checks
    /// within an array algorithm, but when that fails, invoking the
    /// same algorithm on `body`'s argument lets you trade safety for
    /// speed.
    ///
    /// - Warning: Do not rely on anything about `self` (the `ContiguousArray`
    ///   that is the target of this method) during the execution of
    ///   `body`: it may not appear to have its correct value.  Instead,
    ///   use only the `UnsafeMutableBufferPointer` argument to `body`.
    public mutating func withUnsafeMutableBufferPointer<R>(@noescape body: (inout UnsafeMutableBufferPointer<Element>) throws -> R) rethrows -> R
}

extension ContiguousArray {
    /// Replace the given `subRange` of elements with `newElements`.
    ///
    /// - Complexity: O(`subRange.count`) if `subRange.endIndex
    ///   == self.endIndex` and `newElements.isEmpty`, O(N) otherwise.
    public mutating func replaceRange<C : CollectionType where C.Generator.Element == _Buffer.Element>(subRange: Range<Int>, with newElements: C)
}

extension ContiguousArray {
    /// If `!self.isEmpty`, remove the last element and return it, otherwise
    /// return `nil`.
    ///
    /// - Complexity: O(1)
    public mutating func popLast() -> Element?
}

/// A type with a customized textual representation suitable for
/// debugging purposes.
///
/// This textual representation is used when values are written to an
/// *output stream* by `debugPrint`, and is
/// typically more verbose than the text provided by a
/// `CustomStringConvertible`'s `description` property.
///
/// - Note: `String(reflecting: instance)` will work for an `instance`
///   of *any* type, returning its `debugDescription` if the `instance`
///   happens to be `CustomDebugStringConvertible`.  Using
/// `CustomDebugStringConvertible` as a generic constraint, or
/// accessing a conforming type's `debugDescription` directly, is
/// therefore discouraged.
///
/// - SeeAlso: `String.init<T>(reflecting: T)`,
///   `CustomStringConvertible`
public protocol CustomDebugStringConvertible {
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
}

/// A type that explicitly supplies its own Mirror but whose
/// descendant classes are not represented in the Mirror unless they
/// also override `customMirror()`.
public protocol CustomLeafReflectable : CustomReflectable {
}

/// A type that explicitly supplies its own PlaygroundQuickLook.
///
/// Instances of any type can be `PlaygroundQuickLook(reflect:)`'ed
/// upon, but if you are not satisfied with the `PlaygroundQuickLook`
/// supplied for your type by default, you can make it conform to
/// `CustomPlaygroundQuickLookable` and return a custom
/// `PlaygroundQuickLook`.
public protocol CustomPlaygroundQuickLookable {
    /// Return the `Mirror` for `self`.
    ///
    /// - Note: If `Self` has value semantics, the `Mirror` should be
    ///   unaffected by subsequent mutations of `self`.
    @warn_unused_result
    public func customPlaygroundQuickLook() -> PlaygroundQuickLook
}

/// A type that explicitly supplies its own Mirror.
///
/// Instances of any type can be `Mirror(reflect:)`'ed upon, but if you are
/// not satisfied with the `Mirror` supplied for your type by default,
/// you can make it conform to `CustomReflectable` and return a custom
/// `Mirror`.
public protocol CustomReflectable {
    /// Return the `Mirror` for `self`.
    ///
    /// - Note: If `Self` has value semantics, the `Mirror` should be
    ///   unaffected by subsequent mutations of `self`.
    @warn_unused_result
    public func customMirror() -> Mirror
}

/// A type with a customized textual representation.
///
/// This textual representation is used when values are written to an
/// *output stream*, for example, by `print`.
///
/// - Note: `String(instance)` will work for an `instance` of *any*
///   type, returning its `description` if the `instance` happens to be
///   `CustomStringConvertible`.  Using `CustomStringConvertible` as a
///   generic constraint, or accessing a conforming type's `description`
///   directly, is therefore discouraged.
///
/// - SeeAlso: `String.init<T>(T)`, `CustomDebugStringConvertible`
public protocol CustomStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
}

/// A hash-based mapping from `Key` to `Value` instances.  Also a
/// collection of key-value pairs with no defined ordering.
public struct Dictionary<Key : Hashable, Value> : CollectionType, DictionaryLiteralConvertible {
    public typealias Element = (Key, Value)
    public typealias Index = DictionaryIndex<Key, Value>
    /// Create an empty dictionary.
    public init()
    /// Create a dictionary with at least the given number of
    /// elements worth of storage.  The actual capacity will be the
    /// smallest power of 2 that's >= `minimumCapacity`.
    public init(minimumCapacity: Int)
    /// The position of the first element in a non-empty dictionary.
    ///
    /// Identical to `endIndex` in an empty dictionary.
    ///
    /// - Complexity: Amortized O(1) if `self` does not wrap a bridged
    ///   `NSDictionary`, O(N) otherwise.
    public var startIndex: DictionaryIndex<Key, Value> { get }
    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    ///
    /// - Complexity: Amortized O(1) if `self` does not wrap a bridged
    ///   `NSDictionary`, O(N) otherwise.
    public var endIndex: DictionaryIndex<Key, Value> { get }
    /// Returns the `Index` for the given key, or `nil` if the key is not
    /// present in the dictionary.
    @warn_unused_result
    public func indexForKey(key: Key) -> DictionaryIndex<Key, Value>?
    public subscript (position: DictionaryIndex<Key, Value>) -> (Key, Value) { get }
    public subscript (key: Key) -> Value?
    /// Update the value stored in the dictionary for the given key, or, if they
    /// key does not exist, add a new key-value pair to the dictionary.
    ///
    /// Returns the value that was replaced, or `nil` if a new key-value pair
    /// was added.
    public mutating func updateValue(value: Value, forKey key: Key) -> Value?
    /// Remove the key-value pair at `index`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`count`).
    public mutating func removeAtIndex(index: DictionaryIndex<Key, Value>) -> (Key, Value)
    /// Remove a given key and the associated value from the dictionary.
    /// Returns the value that was removed, or `nil` if the key was not present
    /// in the dictionary.
    public mutating func removeValueForKey(key: Key) -> Value?
    /// Remove all elements.
    ///
    /// - Postcondition: `capacity == 0` if `keepCapacity` is `false`, otherwise
    ///   the capacity will not be decreased.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - parameter keepCapacity: If `true`, the operation preserves the
    ///   storage capacity that the collection has, otherwise the underlying
    ///   storage is released.  The default is `false`.
    ///
    /// Complexity: O(`count`).
    public mutating func removeAll(keepCapacity keepCapacity: Bool = default)
    /// The number of entries in the dictionary.
    ///
    /// - Complexity: O(1).
    public var count: Int { get }
    /// Return a *generator* over the (key, value) pairs.
    ///
    /// - Complexity: O(1).
    public func generate() -> DictionaryGenerator<Key, Value>
    /// Create an instance initialized with `elements`.
    public init(dictionaryLiteral elements: (Key, Value)...)
    /// A collection containing just the keys of `self`.
    ///
    /// Keys appear in the same order as they occur as the `.0` member
    /// of key-value pairs in `self`.  Each key in the result has a
    /// unique value.
    public var keys: LazyMapCollection<[Key : Value], Key> { get }
    /// A collection containing just the values of `self`.
    ///
    /// Values appear in the same order as they occur as the `.1` member
    /// of key-value pairs in `self`.
    public var values: LazyMapCollection<[Key : Value], Value> { get }
    /// `true` iff `count == 0`.
    public var isEmpty: Bool { get }
}

extension Dictionary : CustomStringConvertible, CustomDebugStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
}

extension Dictionary : _Reflectable {
}

extension Dictionary {
    /// If `!self.isEmpty`, return the first key-value pair in the sequence of
    /// elements, otherwise return `nil`.
    ///
    /// - Complexity: Amortized O(1)
    public mutating func popFirst() -> (Key, Value)?
}



/// A generator over the members of a `Dictionary<Key, Value>`.
public struct DictionaryGenerator<Key : Hashable, Value> : GeneratorType {
    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: No preceding call to `self.next()` has returned `nil`.
    public mutating func next() -> (Key, Value)?
}

/// Used to access the key-value pairs in an instance of
/// `Dictionary<Key, Value>`.
///
/// Dictionary has two subscripting interfaces:
///
/// 1. Subscripting with a key, yielding an optional value:
///
///        v = d[k]!
///
/// 2. Subscripting with an index, yielding a key-value pair:
///
///        (k,v) = d[i]
public struct DictionaryIndex<Key : Hashable, Value> : ForwardIndexType, Comparable {
    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    public func successor() -> DictionaryIndex<Key, Value>
}

/// Represent the ability to pass a dictionary literal in function
/// signatures.
///
/// A function with a `DictionaryLiteral` parameter can be passed a
/// Swift dictionary literal without causing a `Dictionary` to be
/// created.  This capability can be especially important when the
/// order of elements in the literal is significant.
///
/// For example:
///
///     struct IntPairs {
///       var elements: [(Int, Int)]
///       init(_ pairs: DictionaryLiteral<Int,Int>) {
///         elements = Array(pairs)
///       }
///     }
///
///     let x = IntPairs([1:2, 1:1, 3:4, 2:1])
///     print(x.elements)  // [(1, 2), (1, 1), (3, 4), (2, 1)]
public struct DictionaryLiteral<Key, Value> : DictionaryLiteralConvertible {
    /// Store `elements`.
    public init(dictionaryLiteral elements: (Key, Value)...)
}

extension DictionaryLiteral : CollectionType {
    /// The position of the first element in a non-empty `DictionaryLiteral`.
    ///
    /// Identical to `endIndex` in an empty `DictionaryLiteral`.
    ///
    /// - Complexity: O(1).
    public var startIndex: Int { get }
    /// The `DictionaryLiteral`'s "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    ///
    /// - Complexity: O(1).
    public var endIndex: Int { get }
    public typealias Element = (Key, Value)
    public subscript (position: Int) -> (Key, Value) { get }
}

/// Conforming types can be initialized with dictionary literals.
public protocol DictionaryLiteralConvertible {
    typealias Key
    typealias Value
    /// Create an instance initialized with `elements`.
    public init(dictionaryLiteral elements: (Self.Key, Self.Value)...)
}

public struct Double {
    public var value: Builtin.FPIEEE64
    /// Create an instance initialized to zero.
    public init()
    public init(_bits v: Builtin.FPIEEE64)
    /// Create an instance initialized to `value`.
    public init(_ value: Double)
}

extension Double : CustomStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
}

extension Double : FloatingPointType {
    /// The positive infinity.
    public static var infinity: Double { get }
    /// A quiet NaN.
    public static var NaN: Double { get }
    /// A quiet NaN.
    public static var quietNaN: Double { get }
    /// `true` iff `self` is negative.
    public var isSignMinus: Bool { get }
    /// `true` iff `self` is normal (not zero, subnormal, infinity, or
    /// NaN).
    public var isNormal: Bool { get }
    /// `true` iff `self` is zero, subnormal, or normal (not infinity
    /// or NaN).
    public var isFinite: Bool { get }
    /// `true` iff `self` is +0.0 or -0.0.
    public var isZero: Bool { get }
    /// `true` iff `self` is subnormal.
    public var isSubnormal: Bool { get }
    /// `true` iff `self` is infinity.
    public var isInfinite: Bool { get }
    /// `true` iff `self` is NaN.
    public var isNaN: Bool { get }
    /// `true` iff `self` is a signaling NaN.
    public var isSignaling: Bool { get }
}

extension Double {
    /// The IEEE 754 "class" of this type.
    public var floatingPointClass: FloatingPointClassification { get }
}

extension Double : IntegerLiteralConvertible {
    public init(_builtinIntegerLiteral value: Builtin.Int2048)
    /// Create an instance initialized to `value`.
    public init(integerLiteral value: Int64)
}

extension Double {
    public init(_builtinFloatLiteral value: Builtin.FPIEEE80)
}

extension Double : FloatLiteralConvertible {
    /// Create an instance initialized to `value`.
    public init(floatLiteral value: Double)
}

extension Double : Comparable, Equatable {
}

extension Double : Hashable {
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
}

extension Double : AbsoluteValuable {
    /// Returns the absolute value of `x`.
    @warn_unused_result
    public static func abs(x: Double) -> Double
}

extension Double {
    public init(_ v: UInt8)
    public init(_ v: Int8)
    public init(_ v: UInt16)
    public init(_ v: Int16)
    public init(_ v: UInt32)
    public init(_ v: Int32)
    public init(_ v: UInt64)
    public init(_ v: Int64)
    public init(_ v: UInt)
    public init(_ v: Int)
}

extension Double {
    /// Construct an instance that approximates `other`.
    public init(_ other: Float)
    /// Construct an instance that approximates `other`.
    public init(_ other: Float80)
}

extension Double : Strideable {
    /// Returns a stride `x` such that `self.advancedBy(x)` approximates
    /// `other`.
    ///
    /// - Complexity: O(1).
    public func distanceTo(other: Double) -> Double
    /// Returns a `Self` `x` such that `self.distanceTo(x)` approximates
    /// `n`.
    ///
    /// - Complexity: O(1).
    public func advancedBy(amount: Double) -> Double
}

extension Double {
    /// Construct from an ASCII representation.
    ///
    /// Returns the result of calling the POSIX function
    /// `strtod_l` using the "C" locale, unless
    /// `text` contains non-ASCII text or whitespace, or is not
    /// completely consumed by the call. Otherwise, returns `nil`.
    ///
    /// See the `strtod (3)` man page for details of
    /// the exact format accepted.
    public init?(_ text: String)
}

extension Double : _Reflectable {
}

extension Double : _CVarArgPassedAsDouble, _CVarArgAlignedType {
}

/// A collection whose element type is `Element` but that is always empty.
public struct EmptyCollection<Element> : CollectionType {
    /// A type that represents a valid position in the collection.
    ///
    /// Valid indices consist of the position of every element and a
    /// "past the end" position that's not valid for use as a subscript.
    public typealias Index = Int
    /// Construct an instance.
    public init()
    /// Always zero, just like `endIndex`.
    public var startIndex: Index { get }
    /// Always zero, just like `startIndex`.
    public var endIndex: Index { get }
    /// Returns an empty *generator*.
    ///
    /// - Complexity: O(1).
    public func generate() -> EmptyGenerator<Element>
    public subscript (position: Index) -> Element { get }
    /// Return the number of elements (always zero).
    public var count: Int { get }
}

extension EmptyCollection : _Reflectable {
}

/// A generator that never produces an element.
///
/// - SeeAlso: `EmptyCollection<Element>`.
public struct EmptyGenerator<Element> : GeneratorType, SequenceType {
    /// Construct an instance.
    public init()
    /// Return `nil`, indicating that there are no more elements.
    public mutating func next() -> Element?
}

/// The `GeneratorType` for `EnumerateSequence`.  `EnumerateGenerator`
/// wraps a `Base` `GeneratorType` and yields successive `Int` values,
/// starting at zero, along with the elements of the underlying
/// `Base`:
///
///     var g = EnumerateGenerator(["foo", "bar"].generate())
///     g.next() // (0, "foo")
///     g.next() // (1, "bar")
///     g.next() // nil
///
/// - Note: Idiomatic usage is to call `enumerate` instead of
///   constructing an `EnumerateGenerator` directly.
public struct EnumerateGenerator<Base : GeneratorType> : GeneratorType, SequenceType {
    /// The type of element returned by `next()`.
    public typealias Element = (index: Int, element: Base.Element)
    /// Construct from a `Base` generator.
    public init(_ base: Base)
    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: No preceding call to `self.next()` has returned `nil`.
    public mutating func next() -> (index: Int, element: Base.Element)?
}

/// The `SequenceType` returned by `enumerate()`.  `EnumerateSequence`
/// is a sequence of pairs (*n*, *x*), where *n*s are consecutive
/// `Int`s starting at zero, and *x*s are the elements of a `Base`
/// `SequenceType`:
///
///     var s = EnumerateSequence(["foo", "bar"])
///     Array(s) // [(0, "foo"), (1, "bar")]
///
/// - Note: Idiomatic usage is to call `enumerate` instead of
///   constructing an `EnumerateSequence` directly.
public struct EnumerateSequence<Base : SequenceType> : SequenceType {
    /// Construct from a `Base` sequence.
    public init(_ base: Base)
    /// Returns a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    public func generate() -> EnumerateGenerator<Base.Generator>
}

/// Instances of conforming types can be compared for value equality
/// using operators `==` and `!=`.
///
/// When adopting `Equatable`, only the `==` operator is required to be
/// implemented.  The standard library provides an implementation for `!=`.
public protocol Equatable {
    /// Return true if `lhs` is equal to `rhs`.
    ///
    /// **Equality implies substitutability**.  When `x == y`, `x` and
    /// `y` are interchangeable in any code that only depends on their
    /// values.
    ///
    /// Class instance identity as distinguished by triple-equals `===`
    /// is notably not part of an instance's value.  Exposing other
    /// non-value aspects of `Equatable` types is discouraged, and any
    /// that *are* exposed should be explicitly pointed out in
    /// documentation.
    ///
    /// **Equality is an equivalence relation**
    ///
    /// - `x == x` is `true`
    /// - `x == y` implies `y == x`
    /// - `x == y` and `y == z` implies `x == z`
    ///
    /// **Inequality is the inverse of equality**, i.e. `!(x == y)` iff
    /// `x != y`.
    @warn_unused_result
    public func ==(lhs: Self, rhs: Self) -> Bool
}

public protocol ErrorType {
}

extension ErrorType {
}

/// Conforming types can be initialized with string literals
/// containing a single [Unicode extended grapheme cluster](http://www.unicode.org/glossary/#extended_grapheme_cluster).
public protocol ExtendedGraphemeClusterLiteralConvertible : UnicodeScalarLiteralConvertible {
    typealias ExtendedGraphemeClusterLiteralType
    /// Create an instance initialized to `value`.
    public init(extendedGraphemeClusterLiteral value: Self.ExtendedGraphemeClusterLiteralType)
}

/// The default type for an otherwise-unconstrained Unicode extended
/// grapheme cluster literal.
public typealias ExtendedGraphemeClusterType = String

/// A flattened view of a base collection-of-collections.
///
/// The elements of this view are a concatenation of the elements of
/// each collection in the base.
///
/// The `flatten` property is always lazy, but does not implicitly
/// confer laziness on algorithms applied to its result.  In other
/// words, for ordinary collections `c`:
///
/// * `c.flatten()` does not create new storage
/// * `c.flatten().map(f)` maps eagerly and returns a new array
/// * `c.lazy.flatten().map(f)` maps lazily and returns a `LazyMapCollection`
///
/// - See also: `FlattenSequence`
public struct FlattenBidirectionalCollection<Base : CollectionType where Base.Generator.Element : CollectionType, Base.Index : BidirectionalIndexType, Base.Generator.Element.Index : BidirectionalIndexType> : CollectionType {
    /// A type that represents a valid position in the collection.
    ///
    /// Valid indices consist of the position of every element and a
    /// "past the end" position that's not valid for use as a subscript.
    public typealias Index = FlattenBidirectionalCollectionIndex<Base>
    /// Creates a flattened view of `base`.
    public init(_ base: Base)
    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    public func generate() -> FlattenGenerator<Base.Generator>
    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    public var startIndex: FlattenBidirectionalCollectionIndex<Base> { get }
    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    public var endIndex: FlattenBidirectionalCollectionIndex<Base> { get }
    public subscript (position: FlattenBidirectionalCollectionIndex<Base>) -> Base.Generator.Element.Generator.Element { get }
    public func underestimateCount() -> Int
}

public struct FlattenBidirectionalCollectionIndex<BaseElements : CollectionType where BaseElements.Generator.Element : CollectionType, BaseElements.Index : BidirectionalIndexType, BaseElements.Generator.Element.Index : BidirectionalIndexType> : BidirectionalIndexType {
    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: the next value is representable.
    public func successor() -> FlattenBidirectionalCollectionIndex<BaseElements>
    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    public func predecessor() -> FlattenBidirectionalCollectionIndex<BaseElements>
}

/// A flattened view of a base collection-of-collections.
///
/// The elements of this view are a concatenation of the elements of
/// each collection in the base.
///
/// The `flatten` property is always lazy, but does not implicitly
/// confer laziness on algorithms applied to its result.  In other
/// words, for ordinary collections `c`:
///
/// * `c.flatten()` does not create new storage
/// * `c.flatten().map(f)` maps eagerly and returns a new array
/// * `c.lazy.flatten().map(f)` maps lazily and returns a `LazyMapCollection`
///
/// - See also: `FlattenSequence`
public struct FlattenCollection<Base : CollectionType where Base.Generator.Element : CollectionType> : CollectionType {
    /// A type that represents a valid position in the collection.
    ///
    /// Valid indices consist of the position of every element and a
    /// "past the end" position that's not valid for use as a subscript.
    public typealias Index = FlattenCollectionIndex<Base>
    /// Creates a flattened view of `base`.
    public init(_ base: Base)
    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    public func generate() -> FlattenGenerator<Base.Generator>
    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    public var startIndex: FlattenCollectionIndex<Base> { get }
    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    public var endIndex: FlattenCollectionIndex<Base> { get }
    public subscript (position: FlattenCollectionIndex<Base>) -> Base.Generator.Element.Generator.Element { get }
    public func underestimateCount() -> Int
}

public struct FlattenCollectionIndex<BaseElements : CollectionType where BaseElements.Generator.Element : CollectionType> : ForwardIndexType {
    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: the next value is representable.
    public func successor() -> FlattenCollectionIndex<BaseElements>
}

/// A flattened view of a base generator-of-sequences.
///
/// The elements generated are the concatenation of those in each
/// sequence generated by the base generator.
///
/// - Note: this is the `GeneratorType` used by `FlattenSequence`,
///   `FlattenCollection`, and `BidirectionalFlattenCollection`.
public struct FlattenGenerator<Base : GeneratorType where Base.Element : SequenceType> : GeneratorType, SequenceType {
    /// Construct around a generator for the `base` sequence.
    public init(_ base: Base)
    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: `next()` has not been applied to a copy of `self`
    ///   since the copy was made, and no preceding call to `self.next()`
    ///   has returned `nil`.
    public mutating func next() -> Base.Element.Generator.Element?
}

/// A flattened view of a base sequence-of-sequences.
///
/// The elements of this view are a concatenation of the elements of
/// each sequence in the base.
///
/// The `flatten` property is always lazy, but does not implicitly
/// confer laziness on algorithms applied to its result.  In other
/// words, for ordinary sequences `s`:
///
/// * `s.flatten()` does not create new storage
/// * `s.flatten().map(f)` maps eagerly and returns a new array
/// * `s.lazy.flatten().map(f)` maps lazily and returns a `LazyMapSequence`
///
/// - See also: `FlattenCollection`
public struct FlattenSequence<Base : SequenceType where Base.Generator.Element : SequenceType> : SequenceType {
    /// Creates a concatenation of the elements of the elements of `base`.
    ///
    /// - Complexity: O(1)
    public init(_ base: Base)
    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    public func generate() -> FlattenGenerator<Base.Generator>
}

public struct Float {
    public var value: Builtin.FPIEEE32
    /// Create an instance initialized to zero.
    public init()
    public init(_bits v: Builtin.FPIEEE32)
    /// Create an instance initialized to `value`.
    public init(_ value: Float)
}

extension Float : CustomStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
}

extension Float : FloatingPointType {
    /// The positive infinity.
    public static var infinity: Float { get }
    /// A quiet NaN.
    public static var NaN: Float { get }
    /// A quiet NaN.
    public static var quietNaN: Float { get }
    /// `true` iff `self` is negative.
    public var isSignMinus: Bool { get }
    /// `true` iff `self` is normal (not zero, subnormal, infinity, or
    /// NaN).
    public var isNormal: Bool { get }
    /// `true` iff `self` is zero, subnormal, or normal (not infinity
    /// or NaN).
    public var isFinite: Bool { get }
    /// `true` iff `self` is +0.0 or -0.0.
    public var isZero: Bool { get }
    /// `true` iff `self` is subnormal.
    public var isSubnormal: Bool { get }
    /// `true` iff `self` is infinity.
    public var isInfinite: Bool { get }
    /// `true` iff `self` is NaN.
    public var isNaN: Bool { get }
    /// `true` iff `self` is a signaling NaN.
    public var isSignaling: Bool { get }
}

extension Float {
    /// The IEEE 754 "class" of this type.
    public var floatingPointClass: FloatingPointClassification { get }
}

extension Float : IntegerLiteralConvertible {
    public init(_builtinIntegerLiteral value: Builtin.Int2048)
    /// Create an instance initialized to `value`.
    public init(integerLiteral value: Int64)
}

extension Float {
    public init(_builtinFloatLiteral value: Builtin.FPIEEE80)
}

extension Float : FloatLiteralConvertible {
    /// Create an instance initialized to `value`.
    public init(floatLiteral value: Float)
}

extension Float : Comparable, Equatable {
}

extension Float : Hashable {
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
}

extension Float : AbsoluteValuable {
    /// Returns the absolute value of `x`.
    @warn_unused_result
    public static func abs(x: Float) -> Float
}

extension Float {
    public init(_ v: UInt8)
    public init(_ v: Int8)
    public init(_ v: UInt16)
    public init(_ v: Int16)
    public init(_ v: UInt32)
    public init(_ v: Int32)
    public init(_ v: UInt64)
    public init(_ v: Int64)
    public init(_ v: UInt)
    public init(_ v: Int)
}

extension Float {
    /// Construct an instance that approximates `other`.
    public init(_ other: Double)
    /// Construct an instance that approximates `other`.
    public init(_ other: Float80)
}

extension Float : Strideable {
    /// Returns a stride `x` such that `self.advancedBy(x)` approximates
    /// `other`.
    ///
    /// - Complexity: O(1).
    public func distanceTo(other: Float) -> Float
    /// Returns a `Self` `x` such that `self.distanceTo(x)` approximates
    /// `n`.
    ///
    /// - Complexity: O(1).
    public func advancedBy(amount: Float) -> Float
}

extension Float {
    /// Construct from an ASCII representation.
    ///
    /// Returns the result of calling the POSIX function
    /// `strtof_l` using the "C" locale, unless
    /// `text` contains non-ASCII text or whitespace, or is not
    /// completely consumed by the call. Otherwise, returns `nil`.
    ///
    /// See the `strtof (3)` man page for details of
    /// the exact format accepted.
    public init?(_ text: String)
}

extension Float : _Reflectable {
}

extension Float : _CVarArgPassedAsDouble, _CVarArgAlignedType {
}

/// A 32-bit floating point type.
public typealias Float32 = Float

/// A 64-bit floating point type.
public typealias Float64 = Double

public struct Float80 {
    public var value: Builtin.FPIEEE80
    /// Create an instance initialized to zero.
    public init()
    public init(_bits v: Builtin.FPIEEE80)
    /// Create an instance initialized to `value`.
    public init(_ value: Float80)
}

extension Float80 : CustomStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
}

extension Float80 : IntegerLiteralConvertible {
    public init(_builtinIntegerLiteral value: Builtin.Int2048)
    /// Create an instance initialized to `value`.
    public init(integerLiteral value: Int64)
}

extension Float80 {
    public init(_builtinFloatLiteral value: Builtin.FPIEEE80)
}

extension Float80 : FloatLiteralConvertible {
    /// Create an instance initialized to `value`.
    public init(floatLiteral value: Float80)
}

extension Float80 : Comparable, Equatable {
}

extension Float80 : Hashable {
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
}

extension Float80 : AbsoluteValuable {
    /// Returns the absolute value of `x`.
    @warn_unused_result
    public static func abs(x: Float80) -> Float80
}

extension Float80 {
    public init(_ v: UInt8)
    public init(_ v: Int8)
    public init(_ v: UInt16)
    public init(_ v: Int16)
    public init(_ v: UInt32)
    public init(_ v: Int32)
    public init(_ v: UInt64)
    public init(_ v: Int64)
    public init(_ v: UInt)
    public init(_ v: Int)
}

extension Float80 {
    /// Construct an instance that approximates `other`.
    public init(_ other: Float)
    /// Construct an instance that approximates `other`.
    public init(_ other: Double)
}

extension Float80 : Strideable {
    /// Returns a stride `x` such that `self.advancedBy(x)` approximates
    /// `other`.
    ///
    /// - Complexity: O(1).
    public func distanceTo(other: Float80) -> Float80
    /// Returns a `Self` `x` such that `self.distanceTo(x)` approximates
    /// `n`.
    ///
    /// - Complexity: O(1).
    public func advancedBy(amount: Float80) -> Float80
}

extension Float80 {
    /// Construct from an ASCII representation.
    ///
    /// Returns the result of calling the POSIX function
    /// `strtold_l` using the "C" locale, unless
    /// `text` contains non-ASCII text or whitespace, or is not
    /// completely consumed by the call. Otherwise, returns `nil`.
    ///
    /// See the `strtold (3)` man page for details of
    /// the exact format accepted.
    public init?(_ text: String)
}

/// Conforming types can be initialized with floating point literals.
public protocol FloatLiteralConvertible {
    typealias FloatLiteralType
    /// Create an instance initialized to `value`.
    public init(floatLiteral value: Self.FloatLiteralType)
}

/// The default type for an otherwise-unconstrained floating point literal.
public typealias FloatLiteralType = Double

/// The set of possible IEEE 754 "classes"
public enum FloatingPointClassification {
    case SignalingNaN
    case QuietNaN
    case NegativeInfinity
    case NegativeNormal
    case NegativeSubnormal
    case NegativeZero
    case PositiveZero
    case PositiveSubnormal
    case PositiveNormal
    case PositiveInfinity
}

extension FloatingPointClassification : Equatable {
}

/// A set of common requirements for Swift's floating point types.
public protocol FloatingPointType : Strideable {
    /// Create an instance initialized to `value`.
    public init(_ value: UInt8)
    /// Create an instance initialized to `value`.
    public init(_ value: Int8)
    /// Create an instance initialized to `value`.
    public init(_ value: UInt16)
    /// Create an instance initialized to `value`.
    public init(_ value: Int16)
    /// Create an instance initialized to `value`.
    public init(_ value: UInt32)
    /// Create an instance initialized to `value`.
    public init(_ value: Int32)
    /// Create an instance initialized to `value`.
    public init(_ value: UInt64)
    /// Create an instance initialized to `value`.
    public init(_ value: Int64)
    /// Create an instance initialized to `value`.
    public init(_ value: UInt)
    /// Create an instance initialized to `value`.
    public init(_ value: Int)
    /// The positive infinity.
    public static var infinity: Self { get }
    /// A quiet NaN.
    public static var NaN: Self { get }
    /// A quiet NaN.
    public static var quietNaN: Self { get }
    /// The IEEE 754 "class" of this type.
    public var floatingPointClass: FloatingPointClassification { get }
    /// `true` iff `self` is negative.
    public var isSignMinus: Bool { get }
    /// `true` iff `self` is normal (not zero, subnormal, infinity, or
    /// NaN).
    public var isNormal: Bool { get }
    /// `true` iff `self` is zero, subnormal, or normal (not infinity
    /// or NaN).
    public var isFinite: Bool { get }
    /// `true` iff `self` is +0.0 or -0.0.
    public var isZero: Bool { get }
    /// `true` iff `self` is subnormal.
    public var isSubnormal: Bool { get }
    /// `true` iff `self` is infinity.
    public var isInfinite: Bool { get }
    /// `true` iff `self` is NaN.
    public var isNaN: Bool { get }
    /// `true` iff `self` is a signaling NaN.
    public var isSignaling: Bool { get }
}

/// Represents a discrete value in a series, where a value's
/// successor, if any, is reachable by applying the value's
/// `successor()` method.
public protocol ForwardIndexType : _Incrementable {
    /// A type that can represent the number of steps between pairs of
    /// `Self` values where one value is reachable from the other.
    ///
    /// Reachability is defined by the ability to produce one value from
    /// the other via zero or more applications of `successor`.
    typealias Distance : _SignedIntegerType = Int
    /// Return the result of advancing `self` by `n` positions.
    ///
    /// - Returns:
    ///   - If `n > 0`, the result of applying `successor` to `self` `n` times.
    ///   - If `n < 0`, the result of applying `predecessor` to `self` `-n` times.
    ///   - Otherwise, `self`.
    ///
    /// - Requires: `n >= 0` if only conforming to `ForwardIndexType`
    /// - Complexity:
    ///   - O(1) if conforming to `RandomAccessIndexType`
    ///   - O(`abs(n)`) otherwise
    @warn_unused_result
    public func advancedBy(n: Self.Distance) -> Self
    /// Return the result of advancing `self` by `n` positions, or until it
    /// equals `limit`.
    ///
    /// - Returns:
    ///   - If `n > 0`, the result of applying `successor` to `self` `n` times
    ///     but not past `limit`.
    ///   - If `n < 0`, the result of applying `predecessor` to `self` `-n` times
    ///     but not past `limit`.
    ///   - Otherwise, `self`.
    ///
    /// - Requires: `n >= 0` if only conforming to `ForwardIndexType`.
    ///
    /// - Complexity:
    ///   - O(1) if conforming to `RandomAccessIndexType`
    ///   - O(`abs(n)`) otherwise
    @warn_unused_result
    public func advancedBy(n: Self.Distance, limit: Self) -> Self
    /// Measure the distance between `self` and `end`.
    ///
    /// - Requires:
    ///   - `start` and `end` are part of the same sequence when conforming to
    ///     `RandomAccessSequenceType`.
    ///   - `end` is reachable from `self` by incrementation otherwise.
    ///
    /// - Complexity:
    ///   - O(1) if conforming to `RandomAccessIndexType`
    ///   - O(`n`) otherwise, where `n` is the function's result.
    @warn_unused_result
    public func distanceTo(end: Self) -> Self.Distance
}

extension ForwardIndexType {
    @warn_unused_result
    public func advancedBy(n: Self.Distance) -> Self
    @warn_unused_result
    public func advancedBy(n: Self.Distance, limit: Self) -> Self
    @warn_unused_result
    public func distanceTo(end: Self) -> Self.Distance
}

/// A generator that produces one or fewer instances of `Element`.
public struct GeneratorOfOne<Element> : GeneratorType, SequenceType {
    /// Construct an instance that generates `element!`, or an empty
    /// sequence if `element == nil`.
    public init(_ element: Element?)
    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: `next()` has not been applied to a copy of `self`
    ///   since the copy was made, and no preceding call to `self.next()`
    ///   has returned `nil`.
    public mutating func next() -> Element?
}

/// A sequence built around a generator of type `G`.
///
/// Useful mostly to recover the ability to use `for`...`in`,
/// given just a generator `g`:
///
///     for x in GeneratorSequence(g) { ... }
public struct GeneratorSequence<Base : GeneratorType> : GeneratorType, SequenceType {
    /// Construct an instance whose generator is a copy of `base`.
    public init(_ base: Base)
    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: `next()` has not been applied to a copy of `self`
    ///   since the copy was made, and no preceding call to `self.next()`
    ///   has returned `nil`.
    public mutating func next() -> Base.Element?
}

/// Encapsulates iteration state and interface for iteration over a
/// *sequence*.
///
/// - Note: While it is safe to copy a *generator*, advancing one
///   copy may invalidate the others.
///
/// Any code that uses multiple generators (or `for`...`in` loops)
/// over a single *sequence* should have static knowledge that the
/// specific *sequence* is multi-pass, either because its concrete
/// type is known or because it is constrained to `CollectionType`.
/// Also, the generators must be obtained by distinct calls to the
/// *sequence's* `generate()` method, rather than by copying.
public protocol GeneratorType {
    /// The type of element generated by `self`.
    typealias Element
    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: `next()` has not been applied to a copy of `self`
    ///   since the copy was made, and no preceding call to `self.next()`
    ///   has returned `nil`.  Specific implementations of this protocol
    ///   are encouraged to respond to violations of this requirement by
    ///   calling `preconditionFailure("...")`.
    @warn_unused_result
    public mutating func next() -> Self.Element?
}

/// A half-open `IntervalType`, which contains its `start` but not its
/// `end`.  Can represent an empty interval.
///
/// - parameter Bound: The type of the endpoints.
public struct HalfOpenInterval<Bound : Comparable> : IntervalType, Equatable, CustomStringConvertible, CustomDebugStringConvertible, _Reflectable {
    /// Construct a copy of `x`.
    public init(_ x: HalfOpenInterval<Bound>)
    /// Construct an interval with the given bounds.
    ///
    /// - Requires: `start <= end`.
    public init(_ start: Bound, _ end: Bound)
    /// The `Interval`'s lower bound.
    ///
    /// Invariant: `start` <= `end`.
    public var start: Bound { get }
    /// The `Interval`'s upper bound.
    ///
    /// Invariant: `start` <= `end`.
    public var end: Bound { get }
    /// A textual representation of `self`.
    public var description: String { get }
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
    /// Returns `true` iff the `Interval` contains `x`.
    @warn_unused_result
    public func contains(x: Bound) -> Bool
    /// Returns `intervalToClamp` clamped to `self`.
    ///
    /// The bounds of the result, even if it is empty, are always limited to the bounds of
    /// `self`.
    @warn_unused_result
    public func clamp(intervalToClamp: HalfOpenInterval<Bound>) -> HalfOpenInterval<Bound>
}

extension HalfOpenInterval {
    /// `true` iff the `Interval` is empty.
    public var isEmpty: Bool { get }
}

/// Instances of conforming types provide an integer `hashValue` and
/// can be used as `Dictionary` keys.
public protocol Hashable : Equatable {
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
}

/// An optional type that allows implicit member access (via compiler
/// magic).
///
/// The compiler has special knowledge of the existence of
/// `ImplicitlyUnwrappedOptional<Wrapped>`, but always interacts with it using
/// the library intrinsics below.
public enum ImplicitlyUnwrappedOptional<Wrapped> : _Reflectable, NilLiteralConvertible {
    case None
    case Some(Wrapped)
    /// Construct a `nil` instance.
    public init()
    /// Construct a non-`nil` instance that stores `some`.
    public init(_ some: Wrapped)
    /// Construct an instance from an explicitly unwrapped optional
    /// (`Wrapped?`).
    public init(_ v: Wrapped?)
    /// Create an instance initialized with `nil`.
    public init(nilLiteral: ())
    /// If `self == nil`, returns `nil`.  Otherwise, returns `f(self!)`.
    @warn_unused_result
    public func map<U>(@noescape f: (Wrapped) throws -> U) rethrows -> U!
    /// Returns `nil` if `self` is nil, `f(self!)` otherwise.
    @warn_unused_result
    public func flatMap<U>(@noescape f: (Wrapped) throws -> U!) rethrows -> U!
}

extension ImplicitlyUnwrappedOptional : CustomStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
}

extension ImplicitlyUnwrappedOptional : _ObjectiveCBridgeable {
}

public protocol Indexable {
    /// A type that represents a valid position in the collection.
    ///
    /// Valid indices consist of the position of every element and a
    /// "past the end" position that's not valid for use as a subscript.
    typealias Index : ForwardIndexType
    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    ///
    /// - Complexity: O(1)
    public var startIndex: Self.Index { get }
    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    ///
    /// - Complexity: O(1)
    public var endIndex: Self.Index { get }
    public subscript (position: Self.Index) -> Self._Element { get }
}

/// A *generator* for an arbitrary *collection*.  Provided `C`
/// conforms to the other requirements of `Indexable`,
/// `IndexingGenerator<C>` can be used as the result of `C`'s
/// `generate()` method.  For example:
///
///      struct MyCollection : CollectionType {
///        struct Index : ForwardIndexType { /* implementation hidden */ }
///        subscript(i: Index) -> MyElement { /* implementation hidden */ }
///        func generate() -> IndexingGenerator<MyCollection> { // <===
///          return IndexingGenerator(self)
///        }
///      }
public struct IndexingGenerator<Elements : Indexable> : GeneratorType, SequenceType {
    /// Create a *generator* over the given collection.
    public init(_ elements: Elements)
    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: No preceding call to `self.next()` has returned `nil`.
    public mutating func next() -> Elements._Element?
}

/// A 64-bit signed integer value
/// type.
public struct Int : SignedIntegerType, Comparable, Equatable {
    public var value: Builtin.Int64
    /// A type that can represent the number of steps between pairs of
    /// values.
    public typealias Distance = Int
    /// Create an instance initialized to zero.
    public init()
    public init(_ v: Builtin.Word)
    /// Create an instance initialized to `value`.
    public init(_ value: Int)
    /// Creates an integer from its big-endian representation, changing the
    /// byte order if necessary.
    public init(bigEndian value: Int)
    /// Creates an integer from its little-endian representation, changing the
    /// byte order if necessary.
    public init(littleEndian value: Int)
    public init(_builtinIntegerLiteral value: Builtin.Int2048)
    /// Create an instance initialized to `value`.
    public init(integerLiteral value: Int)
    /// Returns the big-endian representation of the integer, changing the
    /// byte order if necessary.
    public var bigEndian: Int { get }
    /// Returns the little-endian representation of the integer, changing the
    /// byte order if necessary.
    public var littleEndian: Int { get }
    /// Returns the current integer with the byte order swapped.
    public var byteSwapped: Int { get }
    public static var max: Int { get }
    public static var min: Int { get }
}

extension Int : RandomAccessIndexType {
    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    public func successor() -> Int
    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    public func predecessor() -> Int
    public func distanceTo(other: Int) -> Distance
    public func advancedBy(n: Distance) -> Int
}

extension Int : Hashable {
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
}

extension Int : CustomStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
}

extension Int {
    /// Add `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func addWithOverflow(lhs: Int, _ rhs: Int) -> (Int, overflow: Bool)
    /// Subtract `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func subtractWithOverflow(lhs: Int, _ rhs: Int) -> (Int, overflow: Bool)
    /// Multiply `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func multiplyWithOverflow(lhs: Int, _ rhs: Int) -> (Int, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning
    /// a result and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    public static func divideWithOverflow(lhs: Int, _ rhs: Int) -> (Int, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning
    /// the remainder and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    public static func remainderWithOverflow(lhs: Int, _ rhs: Int) -> (Int, overflow: Bool)
    /// Represent this number using Swift's widest native signed
    /// integer type.
    public func toIntMax() -> IntMax
}

extension Int : SignedNumberType {
}

extension Int {
    public init(_ v: UInt8)
    public init(_ v: Int8)
    public init(_ v: UInt16)
    public init(_ v: Int16)
    public init(_ v: UInt32)
    public init(_ v: Int32)
    public init(_ v: UInt64)
    /// Construct a `Int` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: UInt64)
    public init(_ v: Int64)
    /// Construct a `Int` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: Int64)
    public init(_ v: UInt)
    /// Construct a `Int` having the same memory representation as
    /// the `UInt` `bitPattern`.  No range or overflow checking
    /// occurs, and the resulting `Int` may not have the same numeric
    /// value as `bitPattern`--it is only guaranteed to use the same
    /// pattern of bits.
    public init(bitPattern: UInt)
}

extension Int : BitwiseOperationsType {
    /// The empty bitset of type Int.
    public static var allZeros: Int { get }
}

extension Int {
    /// Construct an instance that approximates `other`.
    public init(_ other: Float)
    /// Construct an instance that approximates `other`.
    public init(_ other: Double)
    /// Construct an instance that approximates `other`.
    public init(_ other: Float80)
}

extension Int {
    /// Construct from an ASCII representation in the given `radix`.
    ///
    /// If `text` does not match the regular expression
    /// "[+-][0-9a-zA-Z]+", or the value it denotes in the given `radix`
    /// is not representable, the result is `nil`.
    public init?(_ text: String, radix: Int = default)
}

extension Int : _Reflectable {
}

extension Int : MirrorPathType {
}

extension Int : CVarArgType {
}

/// A 16-bit signed integer value
/// type.
public struct Int16 : SignedIntegerType, Comparable, Equatable {
    public var value: Builtin.Int16
    /// A type that can represent the number of steps between pairs of
    /// values.
    public typealias Distance = Int
    /// Create an instance initialized to zero.
    public init()
    /// Create an instance initialized to `value`.
    public init(_ value: Int16)
    /// Creates an integer from its big-endian representation, changing the
    /// byte order if necessary.
    public init(bigEndian value: Int16)
    /// Creates an integer from its little-endian representation, changing the
    /// byte order if necessary.
    public init(littleEndian value: Int16)
    public init(_builtinIntegerLiteral value: Builtin.Int2048)
    /// Create an instance initialized to `value`.
    public init(integerLiteral value: Int16)
    /// Returns the big-endian representation of the integer, changing the
    /// byte order if necessary.
    public var bigEndian: Int16 { get }
    /// Returns the little-endian representation of the integer, changing the
    /// byte order if necessary.
    public var littleEndian: Int16 { get }
    /// Returns the current integer with the byte order swapped.
    public var byteSwapped: Int16 { get }
    public static var max: Int16 { get }
    public static var min: Int16 { get }
}

extension Int16 : Hashable {
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
}

extension Int16 : CustomStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
}

extension Int16 : RandomAccessIndexType {
    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    public func successor() -> Int16
    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    public func predecessor() -> Int16
    public func distanceTo(other: Int16) -> Distance
    public func advancedBy(n: Distance) -> Int16
}

extension Int16 {
    /// Add `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func addWithOverflow(lhs: Int16, _ rhs: Int16) -> (Int16, overflow: Bool)
    /// Subtract `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func subtractWithOverflow(lhs: Int16, _ rhs: Int16) -> (Int16, overflow: Bool)
    /// Multiply `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func multiplyWithOverflow(lhs: Int16, _ rhs: Int16) -> (Int16, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning
    /// a result and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    public static func divideWithOverflow(lhs: Int16, _ rhs: Int16) -> (Int16, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning
    /// the remainder and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    public static func remainderWithOverflow(lhs: Int16, _ rhs: Int16) -> (Int16, overflow: Bool)
    /// Represent this number using Swift's widest native signed
    /// integer type.
    public func toIntMax() -> IntMax
}

extension Int16 : SignedNumberType {
}

extension Int16 {
    public init(_ v: UInt8)
    public init(_ v: Int8)
    public init(_ v: UInt16)
    public init(_ v: UInt32)
    /// Construct a `Int16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: UInt32)
    public init(_ v: Int32)
    /// Construct a `Int16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: Int32)
    public init(_ v: UInt64)
    /// Construct a `Int16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: UInt64)
    public init(_ v: Int64)
    /// Construct a `Int16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: Int64)
    public init(_ v: UInt)
    /// Construct a `Int16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: UInt)
    public init(_ v: Int)
    /// Construct a `Int16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: Int)
    /// Construct a `Int16` having the same memory representation as
    /// the `UInt16` `bitPattern`.  No range or overflow checking
    /// occurs, and the resulting `Int16` may not have the same numeric
    /// value as `bitPattern`--it is only guaranteed to use the same
    /// pattern of bits.
    public init(bitPattern: UInt16)
}

extension Int16 : BitwiseOperationsType {
    /// The empty bitset of type Int16.
    public static var allZeros: Int16 { get }
}

extension Int16 {
    /// Construct an instance that approximates `other`.
    public init(_ other: Float)
    /// Construct an instance that approximates `other`.
    public init(_ other: Double)
    /// Construct an instance that approximates `other`.
    public init(_ other: Float80)
}

extension Int16 {
    /// Construct from an ASCII representation in the given `radix`.
    ///
    /// If `text` does not match the regular expression
    /// "[+-][0-9a-zA-Z]+", or the value it denotes in the given `radix`
    /// is not representable, the result is `nil`.
    public init?(_ text: String, radix: Int = default)
}

extension Int16 : _Reflectable {
}

extension Int16 : CVarArgType {
}

/// A 32-bit signed integer value
/// type.
public struct Int32 : SignedIntegerType, Comparable, Equatable {
    public var value: Builtin.Int32
    /// A type that can represent the number of steps between pairs of
    /// values.
    public typealias Distance = Int
    /// Create an instance initialized to zero.
    public init()
    /// Create an instance initialized to `value`.
    public init(_ value: Int32)
    /// Creates an integer from its big-endian representation, changing the
    /// byte order if necessary.
    public init(bigEndian value: Int32)
    /// Creates an integer from its little-endian representation, changing the
    /// byte order if necessary.
    public init(littleEndian value: Int32)
    public init(_builtinIntegerLiteral value: Builtin.Int2048)
    /// Create an instance initialized to `value`.
    public init(integerLiteral value: Int32)
    /// Returns the big-endian representation of the integer, changing the
    /// byte order if necessary.
    public var bigEndian: Int32 { get }
    /// Returns the little-endian representation of the integer, changing the
    /// byte order if necessary.
    public var littleEndian: Int32 { get }
    /// Returns the current integer with the byte order swapped.
    public var byteSwapped: Int32 { get }
    public static var max: Int32 { get }
    public static var min: Int32 { get }
}

extension Int32 : Hashable {
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
}

extension Int32 : CustomStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
}

extension Int32 : RandomAccessIndexType {
    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    public func successor() -> Int32
    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    public func predecessor() -> Int32
    public func distanceTo(other: Int32) -> Distance
    public func advancedBy(n: Distance) -> Int32
}

extension Int32 {
    /// Add `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func addWithOverflow(lhs: Int32, _ rhs: Int32) -> (Int32, overflow: Bool)
    /// Subtract `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func subtractWithOverflow(lhs: Int32, _ rhs: Int32) -> (Int32, overflow: Bool)
    /// Multiply `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func multiplyWithOverflow(lhs: Int32, _ rhs: Int32) -> (Int32, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning
    /// a result and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    public static func divideWithOverflow(lhs: Int32, _ rhs: Int32) -> (Int32, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning
    /// the remainder and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    public static func remainderWithOverflow(lhs: Int32, _ rhs: Int32) -> (Int32, overflow: Bool)
    /// Represent this number using Swift's widest native signed
    /// integer type.
    public func toIntMax() -> IntMax
}

extension Int32 : SignedNumberType {
}

extension Int32 {
    public init(_ v: UInt8)
    public init(_ v: Int8)
    public init(_ v: UInt16)
    public init(_ v: Int16)
    public init(_ v: UInt32)
    public init(_ v: UInt64)
    /// Construct a `Int32` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: UInt64)
    public init(_ v: Int64)
    /// Construct a `Int32` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: Int64)
    public init(_ v: UInt)
    /// Construct a `Int32` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: UInt)
    public init(_ v: Int)
    /// Construct a `Int32` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: Int)
    /// Construct a `Int32` having the same memory representation as
    /// the `UInt32` `bitPattern`.  No range or overflow checking
    /// occurs, and the resulting `Int32` may not have the same numeric
    /// value as `bitPattern`--it is only guaranteed to use the same
    /// pattern of bits.
    public init(bitPattern: UInt32)
}

extension Int32 : BitwiseOperationsType {
    /// The empty bitset of type Int32.
    public static var allZeros: Int32 { get }
}

extension Int32 {
    /// Construct an instance that approximates `other`.
    public init(_ other: Float)
    /// Construct an instance that approximates `other`.
    public init(_ other: Double)
    /// Construct an instance that approximates `other`.
    public init(_ other: Float80)
}

extension Int32 {
    /// Construct from an ASCII representation in the given `radix`.
    ///
    /// If `text` does not match the regular expression
    /// "[+-][0-9a-zA-Z]+", or the value it denotes in the given `radix`
    /// is not representable, the result is `nil`.
    public init?(_ text: String, radix: Int = default)
}

extension Int32 : _Reflectable {
}

extension Int32 : CVarArgType {
}

/// A 64-bit signed integer value
/// type.
public struct Int64 : SignedIntegerType, Comparable, Equatable {
    public var value: Builtin.Int64
    /// A type that can represent the number of steps between pairs of
    /// values.
    public typealias Distance = Int
    /// Create an instance initialized to zero.
    public init()
    /// Create an instance initialized to `value`.
    public init(_ value: Int64)
    /// Creates an integer from its big-endian representation, changing the
    /// byte order if necessary.
    public init(bigEndian value: Int64)
    /// Creates an integer from its little-endian representation, changing the
    /// byte order if necessary.
    public init(littleEndian value: Int64)
    public init(_builtinIntegerLiteral value: Builtin.Int2048)
    /// Create an instance initialized to `value`.
    public init(integerLiteral value: Int64)
    /// Returns the big-endian representation of the integer, changing the
    /// byte order if necessary.
    public var bigEndian: Int64 { get }
    /// Returns the little-endian representation of the integer, changing the
    /// byte order if necessary.
    public var littleEndian: Int64 { get }
    /// Returns the current integer with the byte order swapped.
    public var byteSwapped: Int64 { get }
    public static var max: Int64 { get }
    public static var min: Int64 { get }
}

extension Int64 : Hashable {
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
}

extension Int64 : CustomStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
}

extension Int64 : RandomAccessIndexType {
    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    public func successor() -> Int64
    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    public func predecessor() -> Int64
    public func distanceTo(other: Int64) -> Distance
    public func advancedBy(n: Distance) -> Int64
}

extension Int64 {
    /// Add `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func addWithOverflow(lhs: Int64, _ rhs: Int64) -> (Int64, overflow: Bool)
    /// Subtract `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func subtractWithOverflow(lhs: Int64, _ rhs: Int64) -> (Int64, overflow: Bool)
    /// Multiply `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func multiplyWithOverflow(lhs: Int64, _ rhs: Int64) -> (Int64, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning
    /// a result and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    public static func divideWithOverflow(lhs: Int64, _ rhs: Int64) -> (Int64, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning
    /// the remainder and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    public static func remainderWithOverflow(lhs: Int64, _ rhs: Int64) -> (Int64, overflow: Bool)
    /// Represent this number using Swift's widest native signed
    /// integer type.
    public func toIntMax() -> IntMax
}

extension Int64 : SignedNumberType {
}

extension Int64 {
    public init(_ v: UInt8)
    public init(_ v: Int8)
    public init(_ v: UInt16)
    public init(_ v: Int16)
    public init(_ v: UInt32)
    public init(_ v: Int32)
    public init(_ v: UInt64)
    public init(_ v: UInt)
    public init(_ v: Int)
    /// Construct a `Int64` having the same memory representation as
    /// the `UInt64` `bitPattern`.  No range or overflow checking
    /// occurs, and the resulting `Int64` may not have the same numeric
    /// value as `bitPattern`--it is only guaranteed to use the same
    /// pattern of bits.
    public init(bitPattern: UInt64)
}

extension Int64 : BitwiseOperationsType {
    /// The empty bitset of type Int64.
    public static var allZeros: Int64 { get }
}

extension Int64 {
    /// Construct an instance that approximates `other`.
    public init(_ other: Float)
    /// Construct an instance that approximates `other`.
    public init(_ other: Double)
    /// Construct an instance that approximates `other`.
    public init(_ other: Float80)
}

extension Int64 {
    /// Construct from an ASCII representation in the given `radix`.
    ///
    /// If `text` does not match the regular expression
    /// "[+-][0-9a-zA-Z]+", or the value it denotes in the given `radix`
    /// is not representable, the result is `nil`.
    public init?(_ text: String, radix: Int = default)
}

extension Int64 : _Reflectable {
}

extension Int64 : MirrorPathType {
}

extension Int64 : CVarArgType, _CVarArgAlignedType {
}

/// A 8-bit signed integer value
/// type.
public struct Int8 : SignedIntegerType, Comparable, Equatable {
    public var value: Builtin.Int8
    /// A type that can represent the number of steps between pairs of
    /// values.
    public typealias Distance = Int
    /// Create an instance initialized to zero.
    public init()
    /// Create an instance initialized to `value`.
    public init(_ value: Int8)
    public init(_builtinIntegerLiteral value: Builtin.Int2048)
    /// Create an instance initialized to `value`.
    public init(integerLiteral value: Int8)
    public static var max: Int8 { get }
    public static var min: Int8 { get }
}

extension Int8 : Hashable {
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
}

extension Int8 : CustomStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
}

extension Int8 : RandomAccessIndexType {
    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    public func successor() -> Int8
    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    public func predecessor() -> Int8
    public func distanceTo(other: Int8) -> Distance
    public func advancedBy(n: Distance) -> Int8
}

extension Int8 {
    /// Add `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func addWithOverflow(lhs: Int8, _ rhs: Int8) -> (Int8, overflow: Bool)
    /// Subtract `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func subtractWithOverflow(lhs: Int8, _ rhs: Int8) -> (Int8, overflow: Bool)
    /// Multiply `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func multiplyWithOverflow(lhs: Int8, _ rhs: Int8) -> (Int8, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning
    /// a result and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    public static func divideWithOverflow(lhs: Int8, _ rhs: Int8) -> (Int8, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning
    /// the remainder and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    public static func remainderWithOverflow(lhs: Int8, _ rhs: Int8) -> (Int8, overflow: Bool)
    /// Represent this number using Swift's widest native signed
    /// integer type.
    public func toIntMax() -> IntMax
}

extension Int8 : SignedNumberType {
}

extension Int8 {
    public init(_ v: UInt8)
    public init(_ v: UInt16)
    /// Construct a `Int8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: UInt16)
    public init(_ v: Int16)
    /// Construct a `Int8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: Int16)
    public init(_ v: UInt32)
    /// Construct a `Int8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: UInt32)
    public init(_ v: Int32)
    /// Construct a `Int8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: Int32)
    public init(_ v: UInt64)
    /// Construct a `Int8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: UInt64)
    public init(_ v: Int64)
    /// Construct a `Int8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: Int64)
    public init(_ v: UInt)
    /// Construct a `Int8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: UInt)
    public init(_ v: Int)
    /// Construct a `Int8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: Int)
    /// Construct a `Int8` having the same memory representation as
    /// the `UInt8` `bitPattern`.  No range or overflow checking
    /// occurs, and the resulting `Int8` may not have the same numeric
    /// value as `bitPattern`--it is only guaranteed to use the same
    /// pattern of bits.
    public init(bitPattern: UInt8)
}

extension Int8 : BitwiseOperationsType {
    /// The empty bitset of type Int8.
    public static var allZeros: Int8 { get }
}

extension Int8 {
    /// Construct an instance that approximates `other`.
    public init(_ other: Float)
    /// Construct an instance that approximates `other`.
    public init(_ other: Double)
    /// Construct an instance that approximates `other`.
    public init(_ other: Float80)
}

extension Int8 {
    /// Construct from an ASCII representation in the given `radix`.
    ///
    /// If `text` does not match the regular expression
    /// "[+-][0-9a-zA-Z]+", or the value it denotes in the given `radix`
    /// is not representable, the result is `nil`.
    public init?(_ text: String, radix: Int = default)
}

extension Int8 : _Reflectable {
}

extension Int8 : CVarArgType {
}

/// The largest native signed integer type.
public typealias IntMax = Int64

/// The common requirements for types that support integer arithmetic.
public protocol IntegerArithmeticType : _IntegerArithmeticType, Comparable {
    /// Add `lhs` and `rhs`, returning a result and trapping in case of
    /// arithmetic overflow (except in -Ounchecked builds).
    @warn_unused_result
    public func +(lhs: Self, rhs: Self) -> Self
    /// Subtract `lhs` and `rhs`, returning a result and trapping in case of
    /// arithmetic overflow (except in -Ounchecked builds).
    @warn_unused_result
    public func -(lhs: Self, rhs: Self) -> Self
    /// Multiply `lhs` and `rhs`, returning a result and trapping in case of
    /// arithmetic overflow (except in -Ounchecked builds).
    @warn_unused_result
    public func *(lhs: Self, rhs: Self) -> Self
    /// Divide `lhs` and `rhs`, returning a result and trapping in case of
    /// arithmetic overflow (except in -Ounchecked builds).
    @warn_unused_result
    public func /(lhs: Self, rhs: Self) -> Self
    /// Divide `lhs` and `rhs`, returning the remainder and trapping in case of
    /// arithmetic overflow (except in -Ounchecked builds).
    @warn_unused_result
    public func %(lhs: Self, rhs: Self) -> Self
    /// Explicitly convert to `IntMax`, trapping on overflow (except in
    /// -Ounchecked builds).
    @warn_unused_result
    public func toIntMax() -> IntMax
}

/// Conforming types can be initialized with integer literals.
public protocol IntegerLiteralConvertible {
    typealias IntegerLiteralType
    /// Create an instance initialized to `value`.
    public init(integerLiteral value: Self.IntegerLiteralType)
}

/// The default type for an otherwise-unconstrained integer literal.
public typealias IntegerLiteralType = Int

/// A set of common requirements for Swift's integer types.
public protocol IntegerType : _IntegerType, RandomAccessIndexType {
}

/// An interval over a `Comparable` type.
public protocol IntervalType {
    /// The type of the `Interval`'s endpoints.
    typealias Bound : Comparable
    /// Returns `true` iff the interval contains `value`.
    @warn_unused_result
    public func contains(value: Self.Bound) -> Bool
    /// Return `rhs` clamped to `self`.  The bounds of the result, even
    /// if it is empty, are always within the bounds of `self`.
    @warn_unused_result
    public func clamp(intervalToClamp: Self) -> Self
    /// `true` iff `self` is empty.
    public var isEmpty: Bool { get }
    /// The `Interval`'s lower bound.
    ///
    /// Invariant: `start` <= `end`.
    public var start: Self.Bound { get }
    /// The `Interval`'s upper bound.
    ///
    /// Invariant: `start` <= `end`.
    public var end: Self.Bound { get }
}

extension IntervalType {
    /// Returns `true` if `lhs` and `rhs` have a non-empty intersection.
    @warn_unused_result
    public func overlaps<I : IntervalType where I.Bound == Bound>(other: I) -> Bool
}

/// A generator that presents the elements of the sequences generated
/// by `Base`, concatenated using a given separator.
public struct JoinGenerator<Base : GeneratorType where Base.Element : SequenceType> : GeneratorType {
    /// Creates a generator that presents the elements of the sequences
    /// generated by `base`, concatenated using `separator`.
    ///
    /// - Complexity: O(`separator.count`).
    public init<Separator : SequenceType where Separator.Generator.Element == Base.Element.Generator.Element>(base: Base, separator: Separator)
    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    public mutating func next() -> Base.Element.Generator.Element?
}

/// A sequence that presents the elements of the `Base` sequences
/// concatenated using a given separator.
public struct JoinSequence<Base : SequenceType where Base.Generator.Element : SequenceType> : SequenceType {
    /// Creates a sequence that presents the elements of `base` sequences
    /// concatenated using `separator`.
    ///
    /// - Complexity: O(`separator.count`).
    public init<Separator : SequenceType where Separator.Generator.Element == Base.Generator.Element.Generator.Element>(base: Base, separator: Separator)
    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    public func generate() -> JoinGenerator<Base.Generator>
}

/// A collection containing the same elements as a `Base` collection,
/// but on which some operations such as `map` and `filter` are
/// implemented lazily.
///
/// - See also: `LazySequenceType`, `LazyCollection`
public struct LazyCollection<Base : CollectionType> : LazyCollectionType {
    /// The type of the underlying collection
    public typealias Elements = Base
    /// The underlying collection
    public var elements: Base { get }
    /// A type that represents a valid position in the collection.
    ///
    /// Valid indices consist of the position of every element and a
    /// "past the end" position that's not valid for use as a subscript.
    public typealias Index = Base.Index
    /// Construct an instance with `base` as its underlying Collection
    /// instance.
    public init(_ base: Base)
}

extension LazyCollection : SequenceType {
    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    public func generate() -> Base.Generator
    /// Return a value less than or equal to the number of elements in
    /// `self`, **nondestructively**.
    ///
    /// - Complexity: O(N).
    public func underestimateCount() -> Int
}

extension LazyCollection : CollectionType {
    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    public var startIndex: Base.Index { get }
    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    public var endIndex: Base.Index { get }
    public subscript (position: Base.Index) -> Base.Generator.Element { get }
    public subscript (bounds: Range<Base.Index>) -> LazyCollection<Slice<Base>> { get }
    /// Returns `true` iff `self` is empty.
    public var isEmpty: Bool { get }
    /// Returns the number of elements.
    ///
    /// - Complexity: O(1) if `Index` conforms to `RandomAccessIndexType`;
    ///   O(N) otherwise.
    public var count: Base.Index.Distance { get }
    /// Returns the first element of `self`, or `nil` if `self` is empty.
    public var first: Base.Generator.Element? { get }
}

/// A collection on which normally-eager operations such as `map` and
/// `filter` are implemented lazily.
///
/// Please see `LazySequenceType` for background; `LazyCollectionType`
/// is an analogous component, but for collections.
///
/// To add new lazy collection operations, extend this protocol with
/// methods that return lazy wrappers that are themselves
/// `LazyCollectionType`s.
///
/// - See Also: `LazySequenceType`, `LazyCollection`
public protocol LazyCollectionType : CollectionType, LazySequenceType {
    /// A `CollectionType` that can contain the same elements as this one,
    /// possibly with a simpler type.
    ///
    /// - See also: `elements`
    typealias Elements : CollectionType = Self
}

extension LazyCollectionType {
    /// Return the elements of `self` that satisfy `predicate`.
    ///
    /// - Note: The elements of the result are computed on-demand, as
    ///   the result is used. No buffering storage is allocated and each
    ///   traversal step invokes `predicate` on one or more underlying
    ///   elements.
    @warn_unused_result
    public func filter(predicate: (Self.Elements.Generator.Element) -> Bool) -> LazyFilterCollection<Self.Elements>
}

extension LazyCollectionType where Generator.Element : CollectionType, Elements.Generator.Element : CollectionType, Generator.Element == Elements.Generator.Element {
    /// A concatenation of the elements of `self`.
    @warn_unused_result
    public func flatten() -> LazyCollection<FlattenCollection<Self.Elements>>
}

extension LazyCollectionType where Generator.Element : CollectionType, Index : BidirectionalIndexType, Generator.Element.Index : BidirectionalIndexType, Elements.Generator.Element : CollectionType, Elements.Index : BidirectionalIndexType, Elements.Generator.Element.Index : BidirectionalIndexType, Generator.Element == Elements.Generator.Element {
    /// A concatenation of the elements of `self`.
    @warn_unused_result
    public func flatten() -> LazyCollection<FlattenBidirectionalCollection<Self.Elements>>
}

extension LazyCollectionType {
    /// Returns the concatenated results of mapping `transform` over
    /// `self`.  Equivalent to 
    ///
    ///     self.map(transform).flatten()
    ///
    /// - Complexity: O(1)
    @warn_unused_result
    public func flatMap<Intermediate : CollectionType>(transform: (Self.Elements.Generator.Element) -> Intermediate) -> LazyCollection<FlattenCollection<LazyMapCollection<Self.Elements, Intermediate>>>
}

extension LazyCollectionType where Elements.Index : BidirectionalIndexType {
    /// Returns the concatenated results of mapping `transform` over
    /// `self`.  Equivalent to 
    ///
    ///     self.map(transform).flatten()
    ///
    /// - Complexity: O(1)
    @warn_unused_result
    public func flatMap<Intermediate : CollectionType where Intermediate.Index : BidirectionalIndexType>(transform: (Self.Elements.Generator.Element) -> Intermediate) -> LazyCollection<FlattenBidirectionalCollection<LazyMapCollection<Self.Elements, Intermediate>>>
}

extension LazyCollectionType where Elements == Self {
    /// Identical to `self`.
    public var elements: Self { get }
}

extension LazyCollectionType {
    /// Identical to `self`.
    public var lazy: Self { get }
}

extension LazyCollectionType {
    /// Return a `LazyMapCollection` over this `Collection`.  The elements of
    /// the result are computed lazily, each time they are read, by
    /// calling `transform` function on a base element.
    @warn_unused_result
    public func map<U>(transform: (Self.Elements.Generator.Element) -> U) -> LazyMapCollection<Self.Elements, U>
}

extension LazyCollectionType where Index : BidirectionalIndexType, Elements.Index : BidirectionalIndexType {
    /// Return the elements of `self` in reverse order.
    ///
    /// - Complexity: O(1)
    @warn_unused_result
    public func reverse() -> LazyCollection<ReverseCollection<Self.Elements>>
}

extension LazyCollectionType where Index : RandomAccessIndexType, Elements.Index : RandomAccessIndexType {
    /// Return the elements of `self` in reverse order.
    ///
    /// - Complexity: O(1)
    @warn_unused_result
    public func reverse() -> LazyCollection<ReverseRandomAccessCollection<Self.Elements>>
}

/// A lazy `CollectionType` wrapper that includes the elements of an
/// underlying collection that satisfy a predicate.
///
/// - Note: The performance of advancing a `LazyFilterIndex`
///   depends on how sparsely the filtering predicate is satisfied,
///   and may not offer the usual performance given by models of
///   `ForwardIndexType`.  Be aware, therefore, that general operations
///   on `LazyFilterCollection` instances may not have the
///   documented complexity.
public struct LazyFilterCollection<Base : CollectionType> : LazyCollectionType {
    /// A type that represents a valid position in the collection.
    ///
    /// Valid indices consist of the position of every element and a
    /// "past the end" position that's not valid for use as a subscript.
    public typealias Index = LazyFilterIndex<Base>
    /// Construct an instance containing the elements of `base` that
    /// satisfy `predicate`.
    public init(_ base: Base, whereElementsSatisfy predicate: (Base.Generator.Element) -> Bool)
    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    ///
    /// - Complexity: O(N), where N is the ratio between unfiltered and
    ///   filtered collection counts.
    public var startIndex: LazyFilterIndex<Base> { get }
    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    ///
    /// - Complexity: O(1).
    public var endIndex: LazyFilterIndex<Base> { get }
    public subscript (position: LazyFilterIndex<Base>) -> Base.Generator.Element { get }
    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    public func generate() -> LazyFilterGenerator<Base.Generator>
}

/// A generator that produces the elements produced by some base
/// generator that also satisfy a given predicate.
///
/// - Note: This is the associated `Generator` of `LazyFilterSequence`
/// and `LazyFilterCollection`.
public struct LazyFilterGenerator<Base : GeneratorType> : GeneratorType, SequenceType {
    /// Advances to the next element and returns it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: `next()` has not been applied to a copy of `self`
    ///   since the copy was made, and no preceding call to `self.next()`
    ///   has returned `nil`.
    public mutating func next() -> Base.Element?
    /// Creates an instance that produces the elements `x` of `base`
    /// for which `predicate(x) == true`.
    public init(_ base: Base, whereElementsSatisfy predicate: (Base.Element) -> Bool)
    /// The underlying generator whose elements are being filtered
    public var base: Base { get }
}

/// The `Index` used for subscripting a `LazyFilterCollection`.
///
/// The positions of a `LazyFilterIndex` correspond to those positions
/// `p` in its underlying collection `c` such that `c[p]`
/// satisfies the predicate with which the `LazyFilterIndex` was
/// initialized.
/// 
/// - Note: The performance of advancing a `LazyFilterIndex`
///   depends on how sparsely the filtering predicate is satisfied,
///   and may not offer the usual performance given by models of
///   `ForwardIndexType`.
public struct LazyFilterIndex<BaseElements : CollectionType> : ForwardIndexType {
    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    ///
    /// - Complexity: Amortized O(M), where M is the average distance in
    ///   the base collection between elements that satisfy the
    ///   predicate.
    ///
    /// - Note: this operation may not satisfy the expected complexity
    ///   for models of `ForwardIndexType`.
    public func successor() -> LazyFilterIndex<BaseElements>
    /// The position corresponding to `self` in the underlying collection.
    public let base: BaseElements.Index
}

/// A sequence whose elements consist of the elements of some base
/// sequence that also satisfy a given predicate.
///
/// - Note: `s.lazy.filter { ... }`, for an arbitrary sequence `s`,
///   is a `LazyFilterSequence`.
public struct LazyFilterSequence<Base : SequenceType> : LazySequenceType {
    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    public func generate() -> LazyFilterGenerator<Base.Generator>
    /// Creates an instance consisting of the elements `x` of `base` for
    /// which `predicate(x) == true`.
    public init(_ base: Base, whereElementsSatisfy predicate: (Base.Generator.Element) -> Bool)
    /// The underlying sequence whose elements are being filtered
    public let base: Base
}

/// A `CollectionType` whose elements consist of those in a `Base`
/// `CollectionType` passed through a transform function returning `Element`.
/// These elements are computed lazily, each time they're read, by
/// calling the transform function on a base element.
public struct LazyMapCollection<Base : CollectionType, Element> : LazyCollectionType {
    public typealias Index = Base.Index
    public var startIndex: Base.Index { get }
    public var endIndex: Base.Index { get }
    public subscript (position: Base.Index) -> Element { get }
    /// Returns `true` iff `self` is empty.
    public var isEmpty: Bool { get }
    public var first: Element? { get }
    /// Returns a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    public func generate() -> LazyMapGenerator<Base.Generator, Element>
    public func underestimateCount() -> Int
    /// Returns the number of elements.
    ///
    /// - Complexity: O(1) if `Index` conforms to `RandomAccessIndexType`;
    ///   O(N) otherwise.
    public var count: Base.Index.Distance { get }
    /// Create an instance with elements `transform(x)` for each element
    /// `x` of base.
    public init(_ base: Base, transform: (Base.Generator.Element) -> Element)
}

/// The `GeneratorType` used by `MapSequence` and `MapCollection`.
/// Produces each element by passing the output of the `Base`
/// `GeneratorType` through a transform function returning `Element`.
public struct LazyMapGenerator<Base : GeneratorType, Element> : GeneratorType, SequenceType {
    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: `next()` has not been applied to a copy of `self`
    ///   since the copy was made, and no preceding call to `self.next()`
    ///   has returned `nil`.
    public mutating func next() -> Element?
    public var base: Base { get }
}

/// A `SequenceType` whose elements consist of those in a `Base`
/// `SequenceType` passed through a transform function returning `Element`.
/// These elements are computed lazily, each time they're read, by
/// calling the transform function on a base element.
public struct LazyMapSequence<Base : SequenceType, Element> : LazySequenceType {
    public typealias Elements = LazyMapSequence<Base, Element>
    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    public func generate() -> LazyMapGenerator<Base.Generator, Element>
    /// Return a value less than or equal to the number of elements in
    /// `self`, **nondestructively**.
    ///
    /// - Complexity: O(N).
    public func underestimateCount() -> Int
    /// Create an instance with elements `transform(x)` for each element
    /// `x` of base.
    public init(_ base: Base, transform: (Base.Generator.Element) -> Element)
}

/// A sequence containing the same elements as a `Base` sequence, but
/// on which some operations such as `map` and `filter` are
/// implemented lazily.
///
/// - See also: `LazySequenceType`
public struct LazySequence<Base : SequenceType> : LazySequenceType, _SequenceWrapperType {
    /// Creates a sequence that has the same elements as `base`, but on
    /// which some operations such as `map` and `filter` are implemented
    /// lazily.
    public init(_ base: Base)
    /// The `Base` (presumably non-lazy) sequence from which `self` was created.
    public var elements: Base { get }
}

/// A sequence on which normally-eager operations such as `map` and
/// `filter` are implemented lazily.
///
/// Lazy sequences can be used to avoid needless storage allocation
/// and computation, because they use an underlying sequence for
/// storage and compute their elements on demand.  For example,
///
///     [1, 2, 3].lazy.map { $0 * 2 }
///
/// is a sequence containing { `2`, `4`, `6` }.  Each time an element
/// of the lazy sequence is accessed, an element of the underlying
/// array is accessed and transformed by the closure.
///
/// Sequence operations taking closure arguments, such as `map` and
/// `filter`, are normally eager: they use the closure immediately and
/// return a new array.  Using the `lazy` property gives the standard
/// library explicit permission to store the closure and the sequence
/// in the result, and defer computation until it is needed.
///
/// To add new lazy sequence operations, extend this protocol with
/// methods that return lazy wrappers that are themselves
/// `LazySequenceType`s.  For example, given an eager `scan`
/// method defined as follows
///
///     extension SequenceType {
///       /// Returns an array containing the results of
///       ///
///       ///   p.reduce(initial, combine: combine)
///       ///
///       /// for each prefix `p` of `self`, in order from shortest to
///       /// longest.  For example:
///       ///
///       ///     (1..<6).scan(0, combine: +) // [0, 1, 3, 6, 10, 15]
///       ///
///       /// - Complexity: O(N)
///       func scan<ResultElement>(
///         initial: ResultElement,
///         @noescape combine: (ResultElement, Generator.Element)->ResultElement
///       ) -> [ResultElement] {
///         var result = [initial]
///         for x in self {
///           result.append(combine(result.last!, x))
///         }
///         return result
///       }
///     }
///
/// we can build a sequence that lazily computes the elements in the
/// result of `scan`:
///
///     struct LazyScanGenerator<Base: GeneratorType, ResultElement>
///       : GeneratorType {
///       mutating func next() -> ResultElement? {
///         return nextElement.map { result in
///           nextElement = base.next().map { combine(result, $0) }
///           return result
///         }
///       }
///       private var nextElement: ResultElement? // The next result of next().
///       private var base: Base                  // The underlying generator.
///       private let combine: (ResultElement, Base.Element)->ResultElement
///     }
///     
///     struct LazyScanSequence<Base: SequenceType, ResultElement>
///       : LazySequenceType // Chained operations on self are lazy, too
///     {
///       func generate() -> LazyScanGenerator<Base.Generator, ResultElement> {
///         return LazyScanGenerator(
///           nextElement: initial, base: base.generate(), combine: combine)
///       }
///       private let initial: ResultElement
///       private let base: Base
///       private let combine:
///         (ResultElement, Base.Generator.Element)->ResultElement
///     }
///
/// and finally, we can give all lazy sequences a lazy `scan` method:
///     
///     extension LazySequenceType {
///       /// Returns a sequence containing the results of
///       ///
///       ///   p.reduce(initial, combine: combine)
///       ///
///       /// for each prefix `p` of `self`, in order from shortest to
///       /// longest.  For example:
///       ///
///       ///     Array((1..<6).lazy.scan(0, combine: +)) // [0, 1, 3, 6, 10, 15]
///       ///
///       /// - Complexity: O(1)
///       func scan<ResultElement>(
///         initial: ResultElement,
///         combine: (ResultElement, Generator.Element)->ResultElement
///       ) -> LazyScanSequence<Self, ResultElement> {
///         return LazyScanSequence(
///           initial: initial, base: self, combine: combine)
///       }
///     }
///
/// - See also: `LazySequence`, `LazyCollectionType`, `LazyCollection`
///
/// - Note: the explicit permission to implement further operations
///   lazily applies only in contexts where the sequence is statically
///   known to conform to `LazySequenceType`.  Thus, side-effects such
///   as the accumulation of `result` below are never unexpectedly
///   dropped or deferred:
///
///       extension SequenceType where Generator.Element == Int {
///         func sum() -> Int {
///           var result = 0
///           _ = self.map { result += $0 }
///           return result
///         }
///       }
///
///   [We don't recommend that you use `map` this way, because it
///   creates and discards an array. `sum` would be better implemented
///   using `reduce`].
public protocol LazySequenceType : SequenceType {
    /// A `SequenceType` that can contain the same elements as this one,
    /// possibly with a simpler type.
    ///
    /// - See also: `elements`
    typealias Elements : SequenceType = Self
    /// A sequence containing the same elements as this one, possibly with
    /// a simpler type.
    ///
    /// When implementing lazy operations, wrapping `elements` instead
    /// of `self` can prevent result types from growing an extra
    /// `LazySequence` layer.  For example,
    ///
    /// _prext_ example neeeded
    ///
    /// Note: this property need not be implemented by conforming types,
    /// it has a default implementation in a protocol extension that
    /// just returns `self`.
    public var elements: Self.Elements { get }
    public var array: [Self.Generator.Element] { get }
}

extension LazySequenceType {
    /// Return the elements of `self` that satisfy `predicate`.
    ///
    /// - Note: The elements of the result are computed on-demand, as
    ///   the result is used. No buffering storage is allocated and each
    ///   traversal step invokes `predicate` on one or more underlying
    ///   elements.
    @warn_unused_result
    public func filter(predicate: (Self.Elements.Generator.Element) -> Bool) -> LazyFilterSequence<Self.Elements>
}

extension LazySequenceType where Elements.Generator.Element == Generator.Element, Generator.Element : SequenceType {
    /// A concatenation of the elements of `self`.
    @warn_unused_result
    public func flatten() -> LazySequence<FlattenSequence<Self.Elements>>
}

extension LazySequenceType {
    /// Returns the concatenated results of mapping `transform` over
    /// `self`.  Equivalent to 
    ///
    ///     self.map(transform).flatten()
    ///
    /// - Complexity: O(1)
    @warn_unused_result
    public func flatMap<Intermediate : SequenceType>(transform: (Self.Elements.Generator.Element) -> Intermediate) -> LazySequence<FlattenSequence<LazyMapSequence<Self.Elements, Intermediate>>>
}

extension LazySequenceType {
}

extension LazySequenceType where Elements == Self {
    /// Identical to `self`.
    public var elements: Self { get }
}

extension LazySequenceType {
    /// Identical to `self`.
    public var lazy: Self { get }
}

extension LazySequenceType {
    /// Return a `LazyMapSequence` over this `Sequence`.  The elements of
    /// the result are computed lazily, each time they are read, by
    /// calling `transform` function on a base element.
    @warn_unused_result
    public func map<U>(transform: (Self.Elements.Generator.Element) -> U) -> LazyMapSequence<Self.Elements, U>
}

/// A class whose instances contain a property of type `Value` and raw
/// storage for an array of `Element`, whose size is determined at
/// instance creation.
///
/// Note that the `Element` array is suitably-aligned **raw memory**.
/// You are expected to construct and---if necessary---destroy objects
/// there yourself, using the APIs on `UnsafeMutablePointer<Element>`.
/// Typical usage stores a count and capacity in `Value` and destroys
/// any live elements in the `deinit` of a subclass.
/// - Note: Subclasses must not have any stored properties; any storage
///   needed should be included in `Value`.
public class ManagedBuffer<Value, Element> : ManagedProtoBuffer<Value, Element> {
    /// Create a new instance of the most-derived class, calling
    /// `initializeValue` on the partially-constructed object to
    /// generate an initial `Value`.
    final public class func create(minimumCapacity: Int, initialValue: (ManagedProtoBuffer<Value, Element>) -> Value) -> ManagedBuffer<Value, Element>
    /// The stored `Value` instance.
    final public var value: Value
}

/// Contains a buffer object, and provides access to an instance of
/// `Value` and contiguous storage for an arbitrary number of
/// `Element` instances stored in that buffer.
///
/// For most purposes, the `ManagedBuffer` class works fine for this
/// purpose, and can simply be used on its own.  However, in cases
/// where objects of various different classes must serve as storage,
/// `ManagedBufferPointer` is needed.
///
/// A valid buffer class is non-`@objc`, with no declared stored
///   properties.  Its `deinit` must destroy its
///   stored `Value` and any constructed `Element`s.
///
/// Example Buffer Class
/// --------------------
///
///      class MyBuffer<Element> { // non-@objc
///        typealias Manager = ManagedBufferPointer<(Int,String), Element>
///        deinit {
///          Manager(unsafeBufferObject: self).withUnsafeMutablePointers {
///            (pointerToValue, pointerToElements)->Void in
///            pointerToElements.destroy(self.count)
///            pointerToValue.destroy()
///          }
///        }
///
///        // All properties are *computed* based on members of the Value
///        var count: Int {
///          return Manager(unsafeBufferObject: self).value.0
///        }
///        var name: String {
///          return Manager(unsafeBufferObject: self).value.1
///        }
///      }
///
public struct ManagedBufferPointer<Value, Element> : Equatable {
    /// Create with new storage containing an initial `Value` and space
    /// for at least `minimumCapacity` `element`s.
    ///
    /// - parameter bufferClass: The class of the object used for storage.
    /// - parameter minimumCapacity: The minimum number of `Element`s that
    ///   must be able to be stored in the new buffer.
    /// - parameter initialValue: A function that produces the initial
    ///   `Value` instance stored in the buffer, given the `buffer`
    ///   object and a function that can be called on it to get the actual
    ///   number of allocated elements.
    ///
    /// - Requires: `minimumCapacity >= 0`, and the type indicated by
    ///   `bufferClass` is a non-`@objc` class with no declared stored
    ///   properties.  The `deinit` of `bufferClass` must destroy its
    ///   stored `Value` and any constructed `Element`s.
    public init(bufferClass: AnyClass, minimumCapacity: Int, initialValue: (buffer: AnyObject, allocatedCount: (AnyObject) -> Int) -> Value)
    /// Manage the given `buffer`.
    ///
    /// - Requires: `buffer` is an instance of a non-`@objc` class whose
    ///   `deinit` destroys its stored `Value` and any constructed
    ///   `Element`s.
    public init(unsafeBufferObject buffer: AnyObject)
    /// The stored `Value` instance.
    public var value: Value
    /// Return the object instance being used for storage.
    public var buffer: AnyObject { get }
    /// The actual number of elements that can be stored in this object.
    ///
    /// This value may be nontrivial to compute; it is usually a good
    /// idea to store this information in the "value" area when
    /// an instance is created.
    public var allocatedElementCount: Int { get }
    /// Call `body` with an `UnsafeMutablePointer` to the stored
    /// `Value`.
    ///
    /// - Note: This pointer is only valid
    ///   for the duration of the call to `body`.
    public func withUnsafeMutablePointerToValue<R>(body: (UnsafeMutablePointer<Value>) -> R) -> R
    /// Call `body` with an `UnsafeMutablePointer` to the `Element`
    /// storage.
    ///
    /// - Note: This pointer is only valid for the duration of the
    ///   call to `body`.
    public func withUnsafeMutablePointerToElements<R>(body: (UnsafeMutablePointer<Element>) -> R) -> R
    /// Call `body` with `UnsafeMutablePointer`s to the stored `Value`
    /// and raw `Element` storage.
    ///
    /// - Note: These pointers are only valid for the duration of the
    ///   call to `body`.
    public func withUnsafeMutablePointers<R>(body: (UnsafeMutablePointer<Value>, UnsafeMutablePointer<Element>) -> R) -> R
    /// Returns true iff `self` holds the only strong reference to its buffer.
    ///
    /// See `isUniquelyReferenced` for details.
    public mutating func holdsUniqueReference() -> Bool
    /// Returns true iff either `self` holds the only strong reference
    /// to its buffer or the pinned has been 'pinned'.
    ///
    /// See `isUniquelyReferenced` for details.
    public mutating func holdsUniqueOrPinnedReference() -> Bool
}

/// A base class of `ManagedBuffer<Value,Element>`, used during
/// instance creation.
///
/// During instance creation, in particular during
/// `ManagedBuffer.create`'s call to initialize, `ManagedBuffer`'s
/// `value` property is as-yet uninitialized, and therefore
/// `ManagedProtoBuffer` does not offer access to the as-yet
/// uninitialized `value` property of `ManagedBuffer`.
public class ManagedProtoBuffer<Value, Element> : NonObjectiveCBase {
    /// The actual number of elements that can be stored in this object.
    ///
    /// This value may be nontrivial to compute; it is usually a good
    /// idea to store this information in the "value" area when
    /// an instance is created.
    final public var allocatedElementCount: Int { get }
    /// Call `body` with an `UnsafeMutablePointer` to the stored
    /// `Value`.
    ///
    /// - Note: This pointer is only valid for the duration of the
    ///   call to `body`.
    final public func withUnsafeMutablePointerToValue<R>(body: (UnsafeMutablePointer<Value>) -> R) -> R
    /// Call `body` with an `UnsafeMutablePointer` to the `Element`
    /// storage.
    ///
    /// - Note: This pointer is only valid for the duration of the
    ///   call to `body`.
    final public func withUnsafeMutablePointerToElements<R>(body: (UnsafeMutablePointer<Element>) -> R) -> R
    /// Call `body` with `UnsafeMutablePointer`s to the stored `Value`
    /// and raw `Element` storage.
    ///
    /// - Note: These pointers are only valid for the duration of the
    ///   call to `body`.
    final public func withUnsafeMutablePointers<R>(body: (UnsafeMutablePointer<Value>, UnsafeMutablePointer<Element>) -> R) -> R
}

/// Representation of the sub-structure and optional "display style"
/// of any arbitrary subject instance.
///
/// Describes the parts---such as stored properties, collection
/// elements, tuple elements, or the active enumeration case---that
/// make up a particular instance.  May also supply a "display style"
/// property that suggests how this structure might be rendered.
///
/// Mirrors are used by playgrounds and the debugger.
public struct Mirror {
    /// Representation of ancestor classes.
    ///
    /// A `CustomReflectable` class can control how its mirror will
    /// represent ancestor classes by initializing the mirror with a
    /// `AncestorRepresentation`.  This setting has no effect on mirrors
    /// reflecting value type instances.
    public enum AncestorRepresentation {
        /// Generate a default mirror for all ancestor classes.  This is the
        /// default behavior.
        ///
        /// - Note: This option bypasses any implementation of `customMirror`
        ///   that may be supplied by a `CustomReflectable` ancestor, so this
        ///   is typically not the right option for a `customMirror`implementation 
        /// Generate a default mirror for all ancestor classes.
        ///
        /// This case is the default.
        ///
        /// - Note: This option generates default mirrors even for
        ///   ancestor classes that may implement `CustomReflectable`'s
        ///   `customMirror` requirement.  To avoid dropping an ancestor class
        /// customization, an override of `customMirror()` should pass
        /// `ancestorRepresentation: .Customized(super.customMirror)` when
        /// initializing its `Mirror`.
        case Generated
        /// Use the nearest ancestor's implementation of `customMirror()` to
        /// create a mirror for that ancestor.  Other classes derived from
        /// such an ancestor are given a default mirror.
        ///
        /// The payload for this option should always be
        /// "`super.customMirror`":
        ///
        ///     func customMirror() -> Mirror {
        ///       return Mirror(
        ///         self,
        ///         children: ["someProperty": self.someProperty],
        ///         ancestorRepresentation: .Customized(super.customMirror)) // <==
        ///     }
        case Customized(() -> Mirror)
        /// Suppress the representation of all ancestor classes.  The
        /// resulting `Mirror`'s `superclassMirror()` is `nil`.
        case Suppressed
    }
    /// Reflect upon the given `subject`.
    ///
    /// If the dynamic type of `subject` conforms to `CustomReflectable`,
    /// the resulting mirror is determined by its `customMirror` method.
    /// Otherwise, the result is generated by the language.
    ///
    /// - Note: If the dynamic type of `subject` has value semantics,
    ///   subsequent mutations of `subject` will not observable in
    ///   `Mirror`.  In general, though, the observability of such
    /// mutations is unspecified.
    public init(reflecting subject: Any)
    /// An element of the reflected instance's structure.  The optional
    /// `label` may be used when appropriate, e.g. to represent the name
    /// of a stored property or of an active `enum` case, and will be
    /// used for lookup when `String`s are passed to the `descendant`
    /// method.
    public typealias Child = (label: String?, value: Any)
    /// The type used to represent sub-structure.
    ///
    /// Depending on your needs, you may find it useful to "upgrade"
    /// instances of this type to `AnyBidirectionalCollection` or
    /// `AnyRandomAccessCollection`.  For example, to display the last
    /// 20 children of a mirror if they can be accessed efficiently, you
    /// might write:
    ///
    ///     if let b = AnyBidirectionalCollection(someMirror.children) {
    ///       for i in b.endIndex.advancedBy(-20, limit: b.startIndex)..<b.endIndex {
    ///          print(b[i])
    ///       }
    ///     }
    public typealias Children = AnyForwardCollection<Child>
    /// A suggestion of how a `Mirror`'s is to be interpreted.
    ///
    /// Playgrounds and the debugger will show a representation similar
    /// to the one used for instances of the kind indicated by the
    /// `DisplayStyle` case name when the `Mirror` is used for display.
    public enum DisplayStyle {
        case Struct
        case Class
        case Enum
        case Tuple
        case Optional
        case Collection
        case Dictionary
        case Set
    }
    /// Represent `subject` with structure described by `children`,
    /// using an optional `displayStyle`.
    ///
    /// If `subject` is not a class instance, `ancestorRepresentation`
    /// is ignored.  Otherwise, `ancestorRepresentation` determines
    /// whether ancestor classes will be represented and whether their
    /// `customMirror` implementations will be used.  By default, a
    /// representation is automatically generated and any `customMirror`
    /// implementation is bypassed.  To prevent bypassing customized
    /// ancestors, `customMirror` overrides should initialize the
    /// `Mirror` with:
    ///
    ///     ancestorRepresentation: .Customized(super.customMirror)
    ///
    /// - Note: The traversal protocol modeled by `children`'s indices
    ///   (`ForwardIndexType`, `BidirectionalIndexType`, or
    ///   `RandomAccessIndexType`) is captured so that the resulting
    /// `Mirror`'s `children` may be upgraded later.  See the failable
    /// initializers of `AnyBidirectionalCollection` and
    /// `AnyRandomAccessCollection` for details.
    public init<T, C : CollectionType where C.Generator.Element == Child>(_ subject: T, children: C, displayStyle: Mirror.DisplayStyle? = default, ancestorRepresentation: Mirror.AncestorRepresentation = default)
    /// Represent `subject` with child values given by
    /// `unlabeledChildren`, using an optional `displayStyle`.  The
    /// result's child labels will all be `nil`.
    ///
    /// This initializer is especially useful for the mirrors of
    /// collections, e.g.:
    ///
    ///     extension MyArray : CustomReflectable {
    ///       func customMirror() -> Mirror {
    ///         return Mirror(self, unlabeledChildren: self, displayStyle: .Collection)
    ///       }
    ///     }
    ///
    /// If `subject` is not a class instance, `ancestorRepresentation`
    /// is ignored.  Otherwise, `ancestorRepresentation` determines
    /// whether ancestor classes will be represented and whether their
    /// `customMirror` implementations will be used.  By default, a
    /// representation is automatically generated and any `customMirror`
    /// implementation is bypassed.  To prevent bypassing customized
    /// ancestors, `customMirror` overrides should initialize the
    /// `Mirror` with:
    ///
    ///     ancestorRepresentation: .Customized(super.customMirror)
    ///
    /// - Note: The traversal protocol modeled by `children`'s indices
    ///   (`ForwardIndexType`, `BidirectionalIndexType`, or
    ///   `RandomAccessIndexType`) is captured so that the resulting
    /// `Mirror`'s `children` may be upgraded later.  See the failable
    /// initializers of `AnyBidirectionalCollection` and
    /// `AnyRandomAccessCollection` for details.
    public init<T, C : CollectionType>(_ subject: T, unlabeledChildren: C, displayStyle: Mirror.DisplayStyle? = default, ancestorRepresentation: Mirror.AncestorRepresentation = default)
    /// Represent `subject` with labeled structure described by
    /// `children`, using an optional `displayStyle`.
    ///
    /// Pass a dictionary literal with `String` keys as `children`.  Be
    /// aware that although an *actual* `Dictionary` is
    /// arbitrarily-ordered, the ordering of the `Mirror`'s `children`
    /// will exactly match that of the literal you pass.
    ///
    /// If `subject` is not a class instance, `ancestorRepresentation`
    /// is ignored.  Otherwise, `ancestorRepresentation` determines
    /// whether ancestor classes will be represented and whether their
    /// `customMirror` implementations will be used.  By default, a
    /// representation is automatically generated and any `customMirror`
    /// implementation is bypassed.  To prevent bypassing customized
    /// ancestors, `customMirror` overrides should initialize the
    /// `Mirror` with:
    ///
    ///     ancestorRepresentation: .Customized(super.customMirror)
    ///
    /// - Note: The resulting `Mirror`'s `children` may be upgraded to
    ///   `AnyRandomAccessCollection` later.  See the failable
    ///   initializers of `AnyBidirectionalCollection` and
    /// `AnyRandomAccessCollection` for details.
    public init<T>(_ subject: T, children: DictionaryLiteral<String, Any>, displayStyle: Mirror.DisplayStyle? = default, ancestorRepresentation: Mirror.AncestorRepresentation = default)
    /// The static type of the subject being reflected.
    ///
    /// This type may differ from the subject's dynamic type when `self`
    /// is the `superclassMirror()` of another mirror.
    public let subjectType: Any.Type
    /// A collection of `Child` elements describing the structure of the
    /// reflected subject.
    public let children: Children
    /// Suggests a display style for the reflected subject.
    public let displayStyle: Mirror.DisplayStyle?
    @warn_unused_result
    public func superclassMirror() -> Mirror?
}

extension Mirror {
    /// Return a specific descendant of the reflected subject, or `nil`
    /// if no such descendant exists.
    ///
    /// A `String` argument selects the first `Child` with a matching label.
    /// An integer argument *n* select the *n*th `Child`.  For example:
    ///
    ///     var d = Mirror(reflecting: x).descendant(1, "two", 3)
    ///
    /// is equivalent to:
    ///
    ///     var d = nil
    ///     let children = Mirror(reflecting: x).children
    ///     let p0 = children.startIndex.advancedBy(1, limit: children.endIndex)
    ///     if p0 != children.endIndex {
    ///       let grandChildren = Mirror(reflecting: children[p0].value).children
    ///       SeekTwo: for g in grandChildren {
    ///         if g.label == "two" {
    ///           let greatGrandChildren = Mirror(reflecting: g.value).children
    ///           let p1 = greatGrandChildren.startIndex.advancedBy(3,
    ///             limit: greatGrandChildren.endIndex)
    ///           if p1 != endIndex { d = greatGrandChildren[p1].value }
    ///           break SeekTwo
    ///         }
    ///       }
    ///
    /// As you can see, complexity for each element of the argument list
    /// depends on the argument type and capabilities of the collection
    /// used to initialize the corresponding subject's parent's mirror.
    /// Each `String` argument results in a linear search.  In short,
    /// this function is suitable for exploring the structure of a
    /// `Mirror` in a REPL or playground, but don't expect it to be
    /// efficient.
    @warn_unused_result
    public func descendant(first: MirrorPathType, _ rest: MirrorPathType...) -> Any?
}



extension Mirror : CustomStringConvertible {
    public var description: String { get }
}

extension Mirror : CustomReflectable {
    @warn_unused_result
    public func customMirror() -> Mirror
}



/// A protocol for legitimate arguments to `Mirror`'s `descendant`
/// method.
///
/// Do not declare new conformances to this protocol; they will not
/// work as expected.
public protocol MirrorPathType {
}

/// A *collection* that supports subscript assignment.
///
/// For any instance `a` of a type conforming to
/// `MutableCollectionType`, :
///
///     a[i] = x
///     let y = a[i]
///
/// is equivalent to:
///
///     a[i] = x
///     let y = x
///
public protocol MutableCollectionType : MutableIndexable, CollectionType {
    typealias SubSequence = MutableSlice<Self>
    public subscript (position: Self.Index) -> Self.Generator.Element { get set }
    public subscript (bounds: Range<Self.Index>) -> Self.SubSequence { get set }
}

extension MutableCollectionType {
    public subscript (bounds: Range<Self.Index>) -> MutableSlice<Self>
}

extension MutableCollectionType where Index : RandomAccessIndexType {
    /// Re-order the given `range` of elements in `self` and return
    /// a pivot index *p*.
    ///
    /// - Postcondition: for all *i* in `range.startIndex..<`\ *p*, and *j*
    ///   in *p*\ `..<range.endIndex`, `less(self[`\ *i*\ `],
    ///   self[`\ *j*\ `]) && !less(self[`\ *j*\ `], self[`\ *p*\ `])`.
    ///   Only returns `range.endIndex` when `self` is empty.
    ///
    /// - Requires: `isOrderedBefore` is a
    ///   [strict weak ordering](http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings)
    ///   over the elements in `self`.
    public mutating func partition(range: Range<Self.Index>, isOrderedBefore: (Self.Generator.Element, Self.Generator.Element) -> Bool) -> Self.Index
}

extension MutableCollectionType where Index : RandomAccessIndexType, Generator.Element : Comparable {
    /// Re-order the given `range` of elements in `self` and return
    /// a pivot index *p*.
    ///
    /// - Postcondition: for all *i* in `range.startIndex..<`\ *p*, and *j*
    ///   in *p*\ `..<range.endIndex`, `less(self[`\ *i*\ `],
    ///   self[`\ *j*\ `]) && !less(self[`\ *j*\ `], self[`\ *p*\ `])`.
    ///   Only returns `range.endIndex` when `self` is empty.
    ///
    /// - Requires: The less-than operator (`func <`) defined in
    ///   the `Comparable` conformance is a
    ///   [strict weak ordering](http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings)
    ///   over the elements in `self`.
    public mutating func partition(range: Range<Self.Index>) -> Self.Index
}

extension MutableCollectionType where Self.Generator.Element : Comparable {
    /// Return an `Array` containing the sorted elements of `source`.
    ///
    /// The sorting algorithm is not stable (can change the relative order of
    /// elements that compare equal).
    ///
    /// - Requires: The less-than operator (`func <`) defined in
    ///   the `Comparable` conformance is a
    ///   [strict weak ordering](http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings)
    ///   over the elements in `self`.
    @warn_unused_result(mutable_variant="sortInPlace")
    public func sort() -> [Self.Generator.Element]
}

extension MutableCollectionType {
    /// Return an `Array` containing the sorted elements of `source`
    /// according to `isOrderedBefore`.
    ///
    /// The sorting algorithm is not stable (can change the relative order of
    /// elements for which `isOrderedBefore` does not establish an order).
    ///
    /// - Requires: `isOrderedBefore` is a
    ///   [strict weak ordering](http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings)
    ///   over the elements in `self`.
    @warn_unused_result(mutable_variant="sortInPlace")
    public func sort(@noescape isOrderedBefore: (Self.Generator.Element, Self.Generator.Element) -> Bool) -> [Self.Generator.Element]
}

extension MutableCollectionType where Self.Index : RandomAccessIndexType, Self.Generator.Element : Comparable {
    /// Sort `self` in-place.
    ///
    /// The sorting algorithm is not stable (can change the relative order of
    /// elements that compare equal).
    ///
    /// - Requires: The less-than operator (`func <`) defined in
    ///   the `Comparable` conformance is a
    ///   [strict weak ordering](http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings)
    ///   over the elements in `self`.
    public mutating func sortInPlace()
}

extension MutableCollectionType where Self.Index : RandomAccessIndexType {
    /// Sort `self` in-place according to `isOrderedBefore`.
    ///
    /// The sorting algorithm is not stable (can change the relative order of
    /// elements for which `isOrderedBefore` does not establish an order).
    ///
    /// - Requires: `isOrderedBefore` is a
    ///   [strict weak ordering](http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings)
    ///   over the elements in `self`.
    public mutating func sortInPlace(@noescape isOrderedBefore: (Self.Generator.Element, Self.Generator.Element) -> Bool)
}

public protocol MutableIndexable {
    typealias Index : ForwardIndexType
    public var startIndex: Self.Index { get }
    public var endIndex: Self.Index { get }
    public subscript (position: Self.Index) -> Self._Element { get set }
}

/// A view into a sub-sequence of elements of another collection.
///
/// A `MutableSlice` instance stores the base collection, the start and end indices of
/// the view.  It does not copy the elements from the collection into separate
/// storage. Thus, creating a slice has `O(1)` complexity.
///
/// A `MutableSlice` instance inherits the value or reference semantics of the base
/// collection.  That is, if a `MutableSlice` instance is wrapped around a mutable
/// colection that has value semantics (for example, `Array`), mutating the
/// original collection would not affect the copy stored inside of the slice.
///
/// An element of a slice is located under the same index in the slice and in
/// the base collection, as long as neither the collection or the slice were
/// mutated.  Thus, indices of a slice can be used interchangibly with indices
/// of the base collection.
///
/// - Warning: Long-term storage of `MutableSlice` instances is discouraged.
///
///   Because a `MutableSlice` presents a *view* onto the storage of some larger
///   collection even after the original collection goes out of scope, storing
///   the slice may prolong the lifetime of elements that are no longer
///   accessible, which can manifest as apparent memory and object leakage.  To
///   prevent this effect, use slices only for transient computation.
///
/// - Warning: `MutableSlice` requires the setter of `Base.subscript(_: Index)`
///   to not invalidate indices.  If you are writing a collection and mutations
///   need to invalidate indices, don't use `MutableSlice`, use `Slice` or
///   define your own `Base.SubSequence` type that takes that into account.
public struct MutableSlice<Base : MutableIndexable> : MutableCollectionType {
    public typealias Index = Base.Index
    public var startIndex: Base.Index { get }
    public var endIndex: Base.Index { get }
    public subscript (index: Base.Index) -> Base._Element
    public subscript (bounds: Range<Base.Index>) -> MutableSlice<Base>
    public init(base: Base, bounds: Range<Base.Index>)
}

/// A *collection* with mutable slices.
///
/// For example,
///
///      x[i..<j] = someExpression
///      x[i..<j].mutatingMethod()
public protocol MutableSliceable : CollectionType, MutableCollectionType {
    public subscript (_: Range<Self.Index>) -> Self.SubSequence { get set }
}

/// Conforming types can be initialized with `nil`.
public protocol NilLiteralConvertible {
    /// Create an instance initialized with `nil`.
    public init(nilLiteral: ())
}

/// A common base class for classes that need to be non-`@objc`,
/// recognizably in the type system.
///
/// - SeeAlso: `isUniquelyReferenced`
public class NonObjectiveCBase {
    public init()
}

/// A unique identifier for a class instance or metatype. This can be used by
/// reflection clients to recognize cycles in the object graph.
///
/// In Swift, only class instances and metatypes have unique identities. There
/// is no notion of identity for structs, enums, functions, or tuples.
public struct ObjectIdentifier : Hashable, Comparable {
    /// Convert to a `UInt` that captures the full value of `self`.
    ///
    /// Axiom: `a.uintValue == b.uintValue` iff `a == b`.
    public var uintValue: UInt { get }
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
    /// Construct an instance that uniquely identifies the class instance `x`.
    public init(_ x: AnyObject)
    /// Construct an instance that uniquely identifies the metatype `x`.
    public init(_ x: Any.Type)
}

/// Supplies convenient conformance to `SetAlgebraType` for any type
/// whose `RawValue` is a `BitwiseOperationsType`.  For example:
///
///     struct PackagingOptions : OptionSetType {
///       let rawValue: Int
///       init(rawValue: Int) { self.rawValue = rawValue }
///     
///       static let Box = PackagingOptions(rawValue: 1)
///       static let Carton = PackagingOptions(rawValue: 2)
///       static let Bag = PackagingOptions(rawValue: 4)
///       static let Satchel = PackagingOptions(rawValue: 8)
///       static let BoxOrBag: PackagingOptions = [Box, Bag]
///       static let BoxOrCartonOrBag: PackagingOptions = [Box, Carton, Bag]
///     }
///
/// In the example above, `PackagingOptions.Element` is the same type
/// as `PackagingOptions`, and instance `a` subsumes instance `b` if
/// and only if `a.rawValue & b.rawValue == b.rawValue`.
public protocol OptionSetType : SetAlgebraType, RawRepresentable {
    /// An `OptionSet`'s `Element` type is normally `Self`.
    typealias Element = Self
    /// Convert from a value of `RawValue`, succeeding unconditionally.
    public init(rawValue: Self.RawValue)
}

extension OptionSetType {
    /// Returns the set of elements contained in `self`, in `other`, or in
    /// both `self` and `other`.
    @warn_unused_result
    public func union(other: Self) -> Self
    /// Returns the set of elements contained in both `self` and `other`.
    @warn_unused_result
    public func intersect(other: Self) -> Self
    /// Returns the set of elements contained in `self` or in `other`,
    /// but not in both `self` and `other`.
    @warn_unused_result
    public func exclusiveOr(other: Self) -> Self
}

extension OptionSetType where Element == Self {
    /// Returns `true` if `self` contains `member`.
    ///
    /// - Equivalent to `self.intersect([member]) == [member]`
    @warn_unused_result
    public func contains(member: Self) -> Bool
    /// If `member` is not already contained in `self`, insert it.
    ///
    /// - Equivalent to `self.unionInPlace([member])`
    /// - Postcondition: `self.contains(member)`
    public mutating func insert(member: Self)
    /// If `member` is contained in `self`, remove and return it.
    /// Otherwise, return `nil`.
    ///
    /// - Postcondition: `self.intersect([member]).isEmpty`
    public mutating func remove(member: Self) -> Self?
}

extension OptionSetType where RawValue : BitwiseOperationsType {
    /// Create an empty instance.
    ///
    /// - Equivalent to `[] as Self`
    public convenience init()
    /// Insert all elements of `other` into `self`.
    ///
    /// - Equivalent to replacing `self` with `self.union(other)`.
    /// - Postcondition: `self.isSupersetOf(other)`
    public mutating func unionInPlace(other: Self)
    /// Remove all elements of `self` that are not also present in
    /// `other`.
    ///
    /// - Equivalent to replacing `self` with `self.intersect(other)`
    /// - Postcondition: `self.isSubsetOf(other)`
    public mutating func intersectInPlace(other: Self)
    /// Replace `self` with a set containing all elements contained in
    /// either `self` or `other`, but not both.
    ///
    /// - Equivalent to replacing `self` with `self.exclusiveOr(other)`
    public mutating func exclusiveOrInPlace(other: Self)
}

public enum Optional<Wrapped> : _Reflectable, NilLiteralConvertible {
    case None
    case Some(Wrapped)
    /// Construct a `nil` instance.
    public init()
    /// Construct a non-`nil` instance that stores `some`.
    public init(_ some: Wrapped)
    /// If `self == nil`, returns `nil`.  Otherwise, returns `f(self!)`.
    @warn_unused_result
    public func map<U>(@noescape f: (Wrapped) throws -> U) rethrows -> U?
    /// Returns `nil` if `self` is nil, `f(self!)` otherwise.
    @warn_unused_result
    public func flatMap<U>(@noescape f: (Wrapped) throws -> U?) rethrows -> U?
    /// Create an instance initialized with `nil`.
    public init(nilLiteral: ())
}

extension Optional : CustomDebugStringConvertible {
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
}

/// A target of text streaming operations.
public protocol OutputStreamType {
    /// Append the given `string` to this stream.
    public mutating func write(string: String)
}



/// A *generator* that adapts a *collection* `C` and any *sequence* of
/// its `Index` type to present the collection's elements in a
/// permuted order.
public struct PermutationGenerator<C : CollectionType, Indices : SequenceType where C.Index == Indices.Generator.Element> : GeneratorType, SequenceType {
    /// The type of element returned by `next()`.
    public typealias Element = C.Generator.Element
    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: No preceding call to `self.next()` has returned `nil`.
    public mutating func next() -> C.Generator.Element?
    /// Construct a *generator* over a permutation of `elements` given
    /// by `indices`.
    ///
    /// - Requires: `elements[i]` is valid for every `i` in `indices`.
    public init(elements: C, indices: Indices)
}

/// The sum of types that can be used as a quick look representation.
public enum PlaygroundQuickLook {
    /// Plain text.
    case Text(String)
    /// An integer numeric value.
    case Int(Int64)
    /// An unsigned integer numeric value.
    case UInt(UInt64)
    /// A single precision floating-point numeric value.
    case Float(Float32)
    /// A double precision floating-point numeric value.
    case Double(Float64)
    /// An image.
    case Image(Any)
    /// A sound.
    case Sound(Any)
    /// A color.
    case Color(Any)
    /// A bezier path.
    case BezierPath(Any)
    /// An attributed string.
    case AttributedString(Any)
    /// A rectangle.
    ///
    /// Uses explicit coordinates to avoid coupling a particular Cocoa type.
    case Rectangle(Float64, Float64, Float64, Float64)
    /// A point.
    ///
    /// Uses explicit coordinates to avoid coupling a particular Cocoa type.
    case Point(Float64, Float64)
    /// A size.
    ///
    /// Uses explicit coordinates to avoid coupling a particular Cocoa type.
    case Size(Float64, Float64)
    /// A logical value.
    case Logical(Bool)
    /// A range.
    ///
    /// Uses explicit values to avoid coupling a particular Cocoa type.
    case Range(UInt64, UInt64)
    /// A GUI view.
    ///
    /// Uses an Any to avoid coupling a particular Cocoa type.
    case View(Any)
    /// A graphical sprite.
    ///
    /// Uses an Any to avoid coupling a particular Cocoa type.
    case Sprite(Any)
    /// A Uniform Resource Locator.
    case URL(String)
}

extension PlaygroundQuickLook {
    /// Initialize for the given `subject`.
    ///
    /// If the dynamic type of `subject` conforms to
    /// `CustomPlaygroundQuickLookable`, returns the result of calling
    /// its `customPlaygroundQuickLook` method.  Otherwise, returns
    /// a `PlaygroundQuickLook` synthesized for `subject` by the
    /// language.  Note that in some cases the result may be
    /// `.Text(String(reflecting: subject))`.
    ///
    /// - Note: If the dynamic type of `subject` has value semantics,
    ///   subsequent mutations of `subject` will not observable in
    ///   `Mirror`.  In general, though, the observability of such
    /// mutations is unspecified.
    public init(reflecting subject: Any)
}

public enum Process {
    /// The list of command-line arguments with which the current
    /// process was invoked.
    public static let arguments: [String]
    /// Access to the raw argc value from C.
    public static var argc: CInt { get }
    /// Access to the raw argv value from C. Accessing the argument vector
    /// through this pointer is unsafe.
    public static var unsafeArgv: UnsafeMutablePointer<UnsafeMutablePointer<Int8>> { get }
}

/// An *index* that can be offset by an arbitrary number of positions,
/// and can measure the distance to any reachable value, in O(1).
public protocol RandomAccessIndexType : BidirectionalIndexType, Strideable, _RandomAccessAmbiguity {
    @warn_unused_result
    public func distanceTo(other: Self) -> Self.Distance
    @warn_unused_result
    public func advancedBy(n: Self.Distance) -> Self
    @warn_unused_result
    public func advancedBy(n: Self.Distance, limit: Self) -> Self
}

extension RandomAccessIndexType {
    @warn_unused_result
    public func advancedBy(n: Self.Distance, limit: Self) -> Self
}

/// A collection of consecutive discrete index values.
///
/// - parameter Element: Is both the element type and the index type of the
///   collection.
///
/// Like other collections, a range containing one element has an
/// `endIndex` that is the successor of its `startIndex`; and an empty
/// range has `startIndex == endIndex`.
///
/// Axiom: for any `Range` `r`, `r[i] == i`.
///
/// Therefore, if `Element` has a maximal value, it can serve as an
/// `endIndex`, but can never be contained in a `Range<Element>`.
///
/// It also follows from the axiom above that `(-99..<100)[0] == 0`.
/// To prevent confusion (because some expect the result to be `-99`),
/// in a context where `Element` is known to be an integer type,
/// subscripting with `Element` is a compile-time error:
///
///     // error: could not find an overload for 'subscript'...
///     print(Range<Int>(start: -99, end: 100)[0])
///
/// However, subscripting that range still works in a generic context:
///
///     func brackets<Element : ForwardIndexType>(x: Range<Element>, i: Element) -> Element {
///       return x[i] // Just forward to subscript
///     }
///     print(brackets(Range<Int>(start:-99, end:100), 0)) // prints 0
public struct Range<Element : ForwardIndexType> : Equatable, CollectionType, CustomStringConvertible, CustomDebugStringConvertible {
    /// Construct a copy of `x`.
    public init(_ x: Range<Element>)
    /// Construct a range with `startIndex == start` and `endIndex ==
    /// end`.
    public init(start: Element, end: Element)
    public subscript (position: Element) -> Element { get }
    public subscript (_: Element._DisabledRangeIndex) -> Element { get }
    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    public func generate() -> RangeGenerator<Element>
    /// The range's lower bound.
    ///
    /// Identical to `endIndex` in an empty range.
    public var startIndex: Element
    /// The range's upper bound.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    public var endIndex: Element
    /// A textual representation of `self`.
    public var description: String { get }
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
}

extension Range : _Reflectable {
}

/// A generator over the elements of `Range<Element>`.
public struct RangeGenerator<Element : ForwardIndexType> : GeneratorType, SequenceType {
    /// Construct an instance that traverses the elements of `bounds`.
    public init(_ bounds: Range<Element>)
    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    public mutating func next() -> Element?
    /// The lower bound of the remaining range.
    public var startIndex: Element
    /// The upper bound of the remaining range; not included in the
    /// generated sequence.
    public var endIndex: Element
}

/// A *collection* that supports replacement of an arbitrary subRange
/// of elements with the elements of another collection.
public protocol RangeReplaceableCollectionType : CollectionType {
    /// Create an empty instance.
    public init()
    /// Replace the given `subRange` of elements with `newElements`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`subRange.count`) if
    ///   `subRange.endIndex == self.endIndex` and `newElements.isEmpty`,
    ///   O(`self.count` + `newElements.count`) otherwise.
    public mutating func replaceRange<C : CollectionType where C.Generator.Element == Generator.Element>(subRange: Range<Self.Index>, with newElements: C)
    /// A non-binding request to ensure `n` elements of available storage.
    ///
    /// This works as an optimization to avoid multiple reallocations of
    /// linear data structures like `Array`.  Conforming types may
    /// reserve more than `n`, exactly `n`, less than `n` elements of
    /// storage, or even ignore the request completely.
    public mutating func reserveCapacity(n: Self.Index.Distance)
    /// Append `x` to `self`.
    ///
    /// Applying `successor()` to the index of the new element yields
    /// `self.endIndex`.
    ///
    /// - Complexity: Amortized O(1).
    public mutating func append(x: Self.Generator.Element)
    /// Append the elements of `newElements` to `self`.
    ///
    /// - Complexity: O(*length of result*).
    public mutating func appendContentsOf<S : SequenceType where S.Generator.Element == Generator.Element>(newElements: S)
    /// Insert `newElement` at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count`).
    public mutating func insert(newElement: Self.Generator.Element, atIndex i: Self.Index)
    /// Insert `newElements` at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count + newElements.count`).
    public mutating func insertContentsOf<S : CollectionType where S.Generator.Element == Generator.Element>(newElements: S, at i: Self.Index)
    /// Remove the element at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count`).
    public mutating func removeAtIndex(i: Self.Index) -> Self.Generator.Element
    /// Remove the element at `startIndex` and return it.
    ///
    /// - Complexity: O(`self.count`)
    /// - Requires: `!self.isEmpty`.
    public mutating func removeFirst() -> Self.Generator.Element
    /// Remove the first `n` elements.
    ///
    /// - Complexity: O(`self.count`)
    /// - Requires: `self.count >= n`.
    public mutating func removeFirst(n: Int)
    /// Remove the indicated `subRange` of elements.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count`).
    public mutating func removeRange(subRange: Range<Self.Index>)
    /// Remove all elements.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - parameter keepCapacity: If `true`, is a non-binding request to
    ///    avoid releasing storage, which can be a useful optimization
    ///    when `self` is going to be grown again.
    ///
    /// - Complexity: O(`self.count`).
    public mutating func removeAll(keepCapacity keepCapacity: Bool)
}

extension RangeReplaceableCollectionType {
    public mutating func append(newElement: Self.Generator.Element)
    public mutating func appendContentsOf<S : SequenceType where S.Generator.Element == Generator.Element>(newElements: S)
    public mutating func insert(newElement: Self.Generator.Element, atIndex i: Self.Index)
    public mutating func insertContentsOf<C : CollectionType where C.Generator.Element == Generator.Element>(newElements: C, at i: Self.Index)
    public mutating func removeAtIndex(index: Self.Index) -> Self.Generator.Element
    public mutating func removeRange(subRange: Range<Self.Index>)
    public mutating func removeFirst(n: Int)
    public mutating func removeFirst() -> Self.Generator.Element
    public mutating func removeAll(keepCapacity keepCapacity: Bool = default)
    public mutating func reserveCapacity(n: Self.Index.Distance)
}



extension RangeReplaceableCollectionType where Index : BidirectionalIndexType {
    /// Remove an element from the end.
    ///
    /// - Complexity: O(1)
    /// - Requires: `!self.isEmpty`
    public mutating func removeLast() -> Self.Generator.Element
}



/// A byte-sized thing that isn't designed to interoperate with
/// any other types; it makes a decent parameter to
/// `UnsafeMutablePointer<Memory>` when you just want to do bytewise
/// pointer arithmetic.
public struct RawByte {
}

/// A type that can be converted to an associated "raw" type, then
/// converted back to produce an instance equivalent to the original.
public protocol RawRepresentable {
    /// The "raw" type that can be used to represent all values of `Self`.
    ///
    /// Every distinct value of `self` has a corresponding unique
    /// value of `RawValue`, but `RawValue` may have representations
    /// that do not correspond to an value of `Self`.
    typealias RawValue
    /// Convert from a value of `RawValue`, yielding `nil` iff
    /// `rawValue` does not correspond to a value of `Self`.
    public init?(rawValue: Self.RawValue)
    /// The corresponding value of the "raw" type.
    ///
    /// `Self(rawValue: self.rawValue)!` is equivalent to `self`.
    public var rawValue: Self.RawValue { get }
}

/// A collection whose elements are all identical `Element`s.
public struct Repeat<Element> : CollectionType {
    /// A type that represents a valid position in the collection.
    /// 
    /// Valid indices consist of the position of every element and a
    /// "past the end" position that's not valid for use as a subscript.
    public typealias Index = Int
    /// Construct an instance that contains `count` elements having the
    /// value `repeatedValue`.
    public init(count: Int, repeatedValue: Element)
    /// Always zero, which is the index of the first element in a
    /// non-empty instance.
    public var startIndex: Index { get }
    /// Always equal to `count`, which is one greater than the index of
    /// the last element in a non-empty instance.
    public var endIndex: Index { get }
    public subscript (position: Int) -> Element { get }
    /// The number of elements in this collection.
    public var count: Int
    /// The value of every element in this collection.
    public let repeatedValue: Element
}

/// A Collection that presents the elements of its `Base` collection
/// in reverse order.
///
/// - Note: This type is the result of `x.reverse()` where `x` is a
///   collection having bidirectional indices.
///
/// The `reverse()` method is always lazy when applied to a collection
/// with bidirectional indices, but does not implicitly confer
/// laziness on algorithms applied to its result.  In other words, for
/// ordinary collections `c` having bidirectional indices:
///
/// * `c.reverse()` does not create new storage
/// * `c.reverse().map(f)` maps eagerly and returns a new array
/// * `c.lazy.reverse().map(f)` maps lazily and returns a `LazyMapCollection`
///
/// - See also: `ReverseRandomAccessCollection`
public struct ReverseCollection<Base : CollectionType where Base.Index : BidirectionalIndexType> : CollectionType, _ReverseCollectionType {
    /// Creates an instance that presents the elements of `base` in
    /// reverse order.
    ///
    /// - Complexity: O(1)
    public init(_ base: Base)
    /// A type that represents a valid position in the collection.
    ///
    /// Valid indices consist of the position of every element and a
    /// "past the end" position that's not valid for use as a subscript.
    public typealias Index = ReverseIndex<Base.Index>
    /// A type that provides the *sequence*'s iteration interface and
    /// encapsulates its iteration state.
    public typealias Generator = IndexingGenerator<ReverseCollection<Base>>
}

/// A wrapper for a `BidirectionalIndexType` that reverses its
/// direction of traversal.
public struct ReverseIndex<Base : BidirectionalIndexType> : BidirectionalIndexType, ReverseIndexType {
    public typealias Distance = Base.Distance
    public init(_ base: Base)
    /// The successor position in the underlying (un-reversed)
    /// collection.
    ///
    /// If `self` is `advance(c.reverse.startIndex, n)`, then:
    /// - `self.base` is `advance(c.endIndex, -n)`.
    /// - if `n` != `c.count`, then `c.reverse[self]` is 
    ///   equivalent to `[self.base.predecessor()]`.
    public let base: Base
}

public protocol ReverseIndexType : BidirectionalIndexType {
    typealias Base : BidirectionalIndexType
    /// A type that can represent the number of steps between pairs of
    /// `ReverseIndex` values where one value is reachable from the other.
    typealias Distance : _SignedIntegerType = Self.Base.Distance
    /// The successor position in the underlying (un-reversed)
    /// collection.
    ///
    /// If `self` is `advance(c.reverse.startIndex, n)`, then:
    /// - `self.base` is `advance(c.endIndex, -n)`.
    /// - if `n` != `c.count`, then `c.reverse[self]` is 
    ///   equivalent to `[self.base.predecessor()]`.
    public var base: Self.Base { get }
    public init(_ base: Self.Base)
}

/// A Collection that presents the elements of its `Base` collection
/// in reverse order.
///
/// - Note: This type is the result of `x.reverse()` where `x` is a
///   collection having random access indices.
/// - See also: `ReverseCollection`
public struct ReverseRandomAccessCollection<Base : CollectionType where Base.Index : RandomAccessIndexType> : _ReverseCollectionType {
    /// Creates an instance that presents the elements of `base` in
    /// reverse order.
    ///
    /// - Complexity: O(1)
    public init(_ base: Base)
    /// A type that represents a valid position in the collection.
    ///
    /// Valid indices consist of the position of every element and a
    /// "past the end" position that's not valid for use as a subscript.
    public typealias Index = ReverseRandomAccessIndex<Base.Index>
    /// A type that provides the *sequence*'s iteration interface and
    /// encapsulates its iteration state.
    public typealias Generator = IndexingGenerator<ReverseRandomAccessCollection<Base>>
}

/// A wrapper for a `RandomAccessIndexType` that reverses its
/// direction of traversal.
public struct ReverseRandomAccessIndex<Base : RandomAccessIndexType> : RandomAccessIndexType, ReverseIndexType {
    public typealias Distance = Base.Distance
    public init(_ base: Base)
    /// The successor position in the underlying (un-reversed)
    /// collection.
    ///
    /// If `self` is `advance(c.reverse.startIndex, n)`, then:
    /// - `self.base` is `advance(c.endIndex, -n)`.
    /// - if `n` != `c.count`, then `c.reverse[self]` is 
    ///   equivalent to `[self.base.predecessor()]`.
    public let base: Base
    public func distanceTo(other: ReverseRandomAccessIndex<Base>) -> Base.Distance
    public func advancedBy(n: Base.Distance) -> ReverseRandomAccessIndex<Base>
}

/// A type that can be iterated with a `for`...`in` loop.
///
/// `SequenceType` makes no requirement on conforming types regarding
/// whether they will be destructively "consumed" by iteration.  To
/// ensure non-destructive iteration, constrain your *sequence* to
/// `CollectionType`.
///
/// As a consequence, it is not possible to run multiple `for` loops
/// on a sequence to "resume" iteration:
///
///     for element in sequence {
///       if ... some condition { break }
///     }
///
///     for element in sequence {
///       // Not guaranteed to continue from the next element.
///     }
///
/// `SequenceType` makes no requirement about the behavior in that
/// case.  It is not correct to assume that a sequence will either be
/// "consumable" and will resume iteration, or that a sequence is a
/// collection and will restart iteration from the first element.
/// A conforming sequence that is not a collection is allowed to
/// produce an arbitrary sequence of elements from the second generator.
public protocol SequenceType {
    /// A type that provides the *sequence*'s iteration interface and
    /// encapsulates its iteration state.
    typealias Generator : GeneratorType
    /// A type that represents a subsequence of some of the elements.
    typealias SubSequence
    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    @warn_unused_result
    public func generate() -> Self.Generator
    /// Return a value less than or equal to the number of elements in
    /// `self`, **nondestructively**.
    ///
    /// - Complexity: O(N).
    @warn_unused_result
    public func underestimateCount() -> Int
    /// Return an `Array` containing the results of mapping `transform`
    /// over `self`.
    ///
    /// - Complexity: O(N).
    @warn_unused_result
    public func map<T>(@noescape transform: (Self.Generator.Element) throws -> T) rethrows -> [T]
    /// Return an `Array` containing the elements of `self`,
    /// in order, that satisfy the predicate `includeElement`.
    @warn_unused_result
    public func filter(@noescape includeElement: (Self.Generator.Element) throws -> Bool) rethrows -> [Self.Generator.Element]
    /// Call `body` on each element in `self` in the same order as a
    /// *for-in loop.*
    ///
    ///     sequence.forEach {
    ///       // body code
    ///     }
    ///
    /// is similar to:
    ///
    ///     for element in sequence {
    ///       // body code
    ///     }
    ///
    /// - Note: You cannot use the `break` or `continue` statement to exit the
    ///   current call of the `body` closure or skip subsequent calls.
    /// - Note: Using the `return` statement in the `body` closure will only
    ///   exit from the current call to `body`, not any outer scope, and won't
    ///   skip subsequent calls.
    ///
    /// - Complexity: O(`self.count`)
    public func forEach(@noescape body: (Self.Generator.Element) throws -> ()) rethrows
    /// Returns a subsequence containing all but the first `n` elements.
    ///
    /// - Requires: `n >= 0`
    /// - Complexity: O(`n`)
    @warn_unused_result
    public func dropFirst(n: Int) -> Self.SubSequence
    /// Returns a subsequence containing all but the last `n` elements.
    ///
    /// - Requires: `self` is a finite sequence.
    /// - Requires: `n >= 0`
    /// - Complexity: O(`self.count`)
    @warn_unused_result
    public func dropLast(n: Int) -> Self.SubSequence
    /// Returns a subsequence, up to `maxLength` in length, containing the
    /// initial elements.
    ///
    /// If `maxLength` exceeds `self.count`, the result contains all
    /// the elements of `self`.
    ///
    /// - Requires: `maxLength >= 0`
    @warn_unused_result
    public func prefix(maxLength: Int) -> Self.SubSequence
    /// Returns a slice, up to `maxLength` in length, containing the
    /// final elements of `s`.
    ///
    /// If `maxLength` exceeds `s.count`, the result contains all
    /// the elements of `s`.
    ///
    /// - Requires: `self` is a finite sequence.
    /// - Requires: `maxLength >= 0`
    @warn_unused_result
    public func suffix(maxLength: Int) -> Self.SubSequence
    /// Returns the maximal `SubSequence`s of `self`, in order, that
    /// don't contain elements satisfying the predicate `isSeparator`.
    ///
    /// - Parameter maxSplit: The maximum number of `SubSequence`s to
    ///   return, minus 1.
    ///   If `maxSplit + 1` `SubSequence`s are returned, the last one is
    ///   a suffix of `self` containing the remaining elements.
    ///   The default value is `Int.max`.
    ///
    /// - Parameter allowEmptySubsequences: If `true`, an empty `SubSequence`
    ///   is produced in the result for each pair of consecutive elements
    ///   satisfying `isSeparator`.
    ///   The default value is `false`.
    ///
    /// - Requires: `maxSplit >= 0`
    @warn_unused_result
    public func split(maxSplit: Int, allowEmptySlices: Bool, @noescape isSeparator: (Self.Generator.Element) throws -> Bool) rethrows -> [Self.SubSequence]
}



extension SequenceType where Self.Generator.Element : Comparable {
    /// Return an `Array` containing the sorted elements of `source`.
    ///
    /// The sorting algorithm is not stable (can change the relative order of
    /// elements that compare equal).
    ///
    /// - Requires: The less-than operator (`func <`) defined in
    ///   the `Comparable` conformance is a
    ///   [strict weak ordering](http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings)
    ///   over the elements in `self`.
    @warn_unused_result
    public func sort() -> [Self.Generator.Element]
}

extension SequenceType {
    /// Return an `Array` containing the sorted elements of `source`
    /// according to `isOrderedBefore`.
    ///
    /// The sorting algorithm is not stable (can change the relative order of
    /// elements for which `isOrderedBefore` does not establish an order).
    ///
    /// - Requires: `isOrderedBefore` is a
    ///   [strict weak ordering](http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings)
    ///   over the elements in `self`.
    @warn_unused_result
    public func sort(@noescape isOrderedBefore: (Self.Generator.Element, Self.Generator.Element) -> Bool) -> [Self.Generator.Element]
}



extension SequenceType where Generator.Element : SequenceType {
    /// A concatenation of the elements of `self`.
    @warn_unused_result
    public func flatten() -> FlattenSequence<Self>
}

extension SequenceType where Generator.Element : SequenceType {
    /// Returns a view, whose elements are the result of interposing a given
    /// `separator` between the elements of the sequence `self`.
    ///
    /// For example,
    /// `[[1, 2, 3], [4, 5, 6], [7, 8, 9]].joinWithSeparator([-1, -2])`
    /// yields `[1, 2, 3, -1, -2, 4, 5, 6, -1, -2, 7, 8, 9]`.
    @warn_unused_result
    public func joinWithSeparator<Separator : SequenceType where Separator.Generator.Element == Generator.Element.Generator.Element>(separator: Separator) -> JoinSequence<Self>
}

extension SequenceType {
    /// A sequence containing the same elements as a `Base` sequence,
    /// but on which some operations such as `map` and `filter` are
    /// implemented lazily.
    ///
    /// - See also: `LazySequenceType`, `LazySequence`
    public var lazy: LazySequence<Self> { get }
}

extension SequenceType where Self.Generator == Self, Self : GeneratorType {
    public func generate() -> Self
}

extension SequenceType {
    /// Return an `Array` containing the results of mapping `transform`
    /// over `self`.
    ///
    /// - Complexity: O(N).
    @warn_unused_result
    public func map<T>(@noescape transform: (Self.Generator.Element) throws -> T) rethrows -> [T]
    /// Return an `Array` containing the elements of `self`,
    /// in order, that satisfy the predicate `includeElement`.
    @warn_unused_result
    public func filter(@noescape includeElement: (Self.Generator.Element) throws -> Bool) rethrows -> [Self.Generator.Element]
    /// Returns a subsequence containing all but the first `n` elements.
    ///
    /// - Requires: `n >= 0`
    /// - Complexity: O(`n`)
    @warn_unused_result
    public func dropFirst(n: Int) -> AnySequence<Self.Generator.Element>
    /// Returns a subsequence containing all but the last `n` elements.
    ///
    /// - Requires: `self` is a finite collection.
    /// - Requires: `n >= 0`
    /// - Complexity: O(`self.count`)
    @warn_unused_result
    public func dropLast(n: Int) -> AnySequence<Self.Generator.Element>
    @warn_unused_result
    public func prefix(maxLength: Int) -> AnySequence<Self.Generator.Element>
    @warn_unused_result
    public func suffix(maxLength: Int) -> AnySequence<Self.Generator.Element>
    /// Returns the maximal `SubSequence`s of `self`, in order, that
    /// don't contain elements satisfying the predicate `isSeparator`.
    ///
    /// - Parameter maxSplit: The maximum number of `SubSequence`s to
    ///   return, minus 1.
    ///   If `maxSplit + 1` `SubSequence`s are returned, the last one is
    ///   a suffix of `self` containing the remaining elements.
    ///   The default value is `Int.max`.
    ///
    /// - Parameter allowEmptySubsequences: If `true`, an empty `SubSequence`
    ///   is produced in the result for each pair of consecutive elements
    ///   satisfying `isSeparator`.
    ///   The default value is `false`.
    ///
    /// - Requires: `maxSplit >= 0`
    @warn_unused_result
    public func split(maxSplit: Int = default, allowEmptySlices: Bool = default, @noescape isSeparator: (Self.Generator.Element) throws -> Bool) rethrows -> [AnySequence<Self.Generator.Element>]
    /// Return a value less than or equal to the number of elements in
    /// `self`, **nondestructively**.
    ///
    /// - Complexity: O(N).
    @warn_unused_result
    public func underestimateCount() -> Int
}

extension SequenceType {
    /// Call `body` on each element in `self` in the same order as a
    /// *for-in loop.*
    ///
    ///     sequence.forEach {
    ///       // body code
    ///     }
    ///
    /// is similar to:
    ///
    ///     for element in sequence {
    ///       // body code
    ///     }
    ///
    /// - Note: You cannot use the `break` or `continue` statement to exit the
    ///   current call of the `body` closure or skip subsequent calls.
    /// - Note: Using the `return` statement in the `body` closure will only
    ///   exit from the current call to `body`, not any outer scope, and won't
    ///   skip subsequent calls.
    ///
    /// - Complexity: O(`self.count`)
    public func forEach(@noescape body: (Self.Generator.Element) throws -> ()) rethrows
}

extension SequenceType where Generator.Element : Equatable {
    /// Returns the maximal `SubSequence`s of `self`, in order, around elements
    /// equatable to `separator`.
    ///
    /// - Parameter maxSplit: The maximum number of `SubSequence`s to
    ///   return, minus 1.
    ///   If `maxSplit + 1` `SubSequence`s are returned, the last one is
    ///   a suffix of `self` containing the remaining elements.
    ///   The default value is `Int.max`.
    ///
    /// - Parameter allowEmptySubsequences: If `true`, an empty `SubSequence`
    ///   is produced in the result for each pair of consecutive elements
    ///   satisfying `isSeparator`.
    ///   The default value is `false`.
    ///
    /// - Requires: `maxSplit >= 0`
    @warn_unused_result
    public func split(separator: Self.Generator.Element, maxSplit: Int = default, allowEmptySlices: Bool = default) -> [AnySequence<Self.Generator.Element>]
}

extension SequenceType {
    /// Returns a subsequence containing all but the first element.
    ///
    /// - Requires: `n >= 0`
    /// - Complexity: O(`n`)
    @warn_unused_result
    public func dropFirst() -> Self.SubSequence
    /// Returns a subsequence containing all but the last element.
    ///
    /// - Requires: `self` is a finite sequence.
    /// - Requires: `n >= 0`
    /// - Complexity: O(`self.count`)
    @warn_unused_result
    public func dropLast() -> Self.SubSequence
}



extension SequenceType where Self : _SequenceWrapperType, Self.Generator == Self.Base.Generator {
    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    public func generate() -> Self.Generator
    public func underestimateCount() -> Int
    @warn_unused_result
    public func map<T>(@noescape transform: (Self.Generator.Element) throws -> T) rethrows -> [T]
    @warn_unused_result
    public func filter(@noescape includeElement: (Self.Generator.Element) throws -> Bool) rethrows -> [Self.Generator.Element]
}

extension SequenceType {
    /// Return a lazy `SequenceType` containing pairs (*n*, *x*), where
    /// *n*s are consecutive `Int`s starting at zero, and *x*s are
    /// the elements of `base`:
    ///
    ///     > for (n, c) in "Swift".characters.enumerate() {
    ///         print("\(n): '\(c)'")
    ///       }
    ///     0: 'S'
    ///     1: 'w'
    ///     2: 'i'
    ///     3: 'f'
    ///     4: 't'
    @warn_unused_result
    public func enumerate() -> EnumerateSequence<Self>
}

extension SequenceType {
    /// Returns the minimum element in `self` or `nil` if the sequence is empty.
    ///
    /// - Complexity: O(`elements.count`).
    ///
    /// 
    /// - Requires: `isOrderedBefore` is a
    ///   [strict weak ordering](http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings).
    ///   over `self`.
    @warn_unused_result
    public func minElement(@noescape isOrderedBefore: (Self.Generator.Element, Self.Generator.Element) throws -> Bool) rethrows -> Self.Generator.Element?
    /// Returns the maximum element in `self` or `nil` if the sequence is empty.
    ///
    /// - Complexity: O(`elements.count`).
    ///   
    /// - Requires: `isOrderedBefore` is a
    ///   [strict weak ordering](http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings).
    ///   over `self`.
    @warn_unused_result
    public func maxElement(@noescape isOrderedBefore: (Self.Generator.Element, Self.Generator.Element) throws -> Bool) rethrows -> Self.Generator.Element?
}

extension SequenceType where Generator.Element : Comparable {
    /// Returns the minimum element in `self` or `nil` if the sequence is empty.
    ///
    /// - Complexity: O(`elements.count`).
    ///
    /// 
    @warn_unused_result
    public func minElement() -> Self.Generator.Element?
    /// Returns the maximum element in `self` or `nil` if the sequence is empty.
    ///
    /// - Complexity: O(`elements.count`).
    ///   
    @warn_unused_result
    public func maxElement() -> Self.Generator.Element?
}

extension SequenceType {
    /// Return true iff `self` begins with elements equivalent to those of
    /// `other`, using `isEquivalent` as the equivalence test.  Return true if
    /// `other` is empty.
    ///
    /// - Requires: `isEquivalent` is an
    ///   [equivalence relation](http://en.wikipedia.org/wiki/Equivalence_relation).
    @warn_unused_result
    public func startsWith<OtherSequence : SequenceType where OtherSequence.Generator.Element == Generator.Element>(other: OtherSequence, @noescape isEquivalent: (Self.Generator.Element, Self.Generator.Element) throws -> Bool) rethrows -> Bool
}

extension SequenceType where Generator.Element : Equatable {
    /// Return true iff the the initial elements of `self` are equal to `prefix`.
    /// Return true if `other` is empty.
    @warn_unused_result
    public func startsWith<OtherSequence : SequenceType where OtherSequence.Generator.Element == Generator.Element>(other: OtherSequence) -> Bool
}

extension SequenceType {
    /// Return true iff `self` and `other` contain equivalent elements, using
    /// `isEquivalent` as the equivalence test.
    ///
    /// - Requires: `isEquivalent` is an
    ///   [equivalence relation](http://en.wikipedia.org/wiki/Equivalence_relation).
    @warn_unused_result
    public func elementsEqual<OtherSequence : SequenceType where OtherSequence.Generator.Element == Generator.Element>(other: OtherSequence, @noescape isEquivalent: (Self.Generator.Element, Self.Generator.Element) throws -> Bool) rethrows -> Bool
}

extension SequenceType where Generator.Element : Equatable {
    /// Return `true` iff `self` and `other` contain the same elements in the
    /// same order.
    @warn_unused_result
    public func elementsEqual<OtherSequence : SequenceType where OtherSequence.Generator.Element == Generator.Element>(other: OtherSequence) -> Bool
}

extension SequenceType {
    /// Return true iff `self` precedes `other` in a lexicographical ("dictionary")
    /// ordering, using `isOrderedBefore` as the comparison between elements.
    ///
    /// - Note: This method implements the mathematical notion of lexicographical
    ///   ordering, which has no connection to Unicode.  If you are sorting strings
    ///   to present to the end-user, you should use `String` APIs that perform
    /// localized comparison.
    ///
    /// - Requires: `isOrderedBefore` is a
    ///   [strict weak ordering](http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings)
    ///   over the elements of `self` and `other`.
    @warn_unused_result
    public func lexicographicalCompare<OtherSequence : SequenceType where OtherSequence.Generator.Element == Generator.Element>(other: OtherSequence, @noescape isOrderedBefore: (Self.Generator.Element, Self.Generator.Element) throws -> Bool) rethrows -> Bool
}

extension SequenceType where Generator.Element : Comparable {
    /// Return true iff `self` precedes `other` in a lexicographical ("dictionary")
    /// ordering, using "<" as the comparison between elements.
    ///
    /// - Note: This method implements the mathematical notion of lexicographical
    ///   ordering, which has no connection to Unicode.  If you are sorting strings
    ///   to present to the end-user, you should use `String` APIs that perform
    /// localized comparison.
    @warn_unused_result
    public func lexicographicalCompare<OtherSequence : SequenceType where OtherSequence.Generator.Element == Generator.Element>(other: OtherSequence) -> Bool
}

extension SequenceType where Generator.Element : Equatable {
    /// Return `true` iff `element` is in `self`.
    @warn_unused_result
    public func contains(element: Self.Generator.Element) -> Bool
}

extension SequenceType {
    /// Return `true` iff an element in `self` satisfies `predicate`.
    @warn_unused_result
    public func contains(@noescape predicate: (Self.Generator.Element) throws -> Bool) rethrows -> Bool
}

extension SequenceType {
    /// Return the result of repeatedly calling `combine` with an
    /// accumulated value initialized to `initial` and each element of
    /// `self`, in turn, i.e. return
    /// `combine(combine(...combine(combine(initial, self[0]),
    /// self[1]),...self[count-2]), self[count-1])`.
    @warn_unused_result
    public func reduce<T>(initial: T, @noescape combine: (T, Self.Generator.Element) throws -> T) rethrows -> T
}

extension SequenceType {
    /// Return an `Array` containing the elements of `self` in reverse
    /// order.
    ///
    /// Complexity: O(N), where N is the length of `self`.
    @warn_unused_result
    public func reverse() -> [Self.Generator.Element]
}

extension SequenceType {
    /// Return an `Array` containing the concatenated results of mapping
    /// `transform` over `self`.
    ///
    ///     s.flatMap(transform)
    ///
    /// is equivalent to
    ///
    ///     Array(s.map(transform).flatten())
    ///
    /// - Complexity: O(*M* + *N*), where *M* is the length of `self`
    ///   and *N* is the length of the result.
    @warn_unused_result
    public func flatMap<S : SequenceType>(transform: (Self.Generator.Element) throws -> S) rethrows -> [S.Generator.Element]
}

extension SequenceType {
    /// Return an `Array` containing the non-nil results of mapping
    /// `transform` over `self`.
    ///
    /// - Complexity: O(*M* + *N*), where *M* is the length of `self`
    ///   and *N* is the length of the result.
    @warn_unused_result
    public func flatMap<T>(@noescape transform: (Self.Generator.Element) throws -> T?) rethrows -> [T]
}

extension SequenceType where Generator.Element == String {
    /// Interpose the `separator` between elements of `self`, then concatenate
    /// the result.  For example:
    ///
    ///     ["foo", "bar", "baz"].joinWithSeparator("-|-") // "foo-|-bar-|-baz"
    @warn_unused_result
    public func joinWithSeparator(separator: String) -> String
}

/// A collection of unique `Element` instances with no defined ordering.
public struct Set<Element : Hashable> : Hashable, CollectionType, ArrayLiteralConvertible {
    public typealias Index = SetIndex<Element>
    /// Create an empty set with at least the given number of
    /// elements worth of storage.  The actual capacity will be the
    /// smallest power of 2 that's >= `minimumCapacity`.
    public init(minimumCapacity: Int)
    /// The position of the first element in a non-empty set.
    ///
    /// This is identical to `endIndex` in an empty set.
    ///
    /// - Complexity: Amortized O(1) if `self` does not wrap a bridged
    ///   `NSSet`, O(N) otherwise.
    public var startIndex: SetIndex<Element> { get }
    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    ///
    /// - Complexity: Amortized O(1) if `self` does not wrap a bridged
    ///   `NSSet`, O(N) otherwise.
    public var endIndex: SetIndex<Element> { get }
    /// Returns `true` if the set contains a member.
    @warn_unused_result
    public func contains(member: Element) -> Bool
    /// Returns the `Index` of a given member, or `nil` if the member is not
    /// present in the set.
    @warn_unused_result
    public func indexOf(member: Element) -> SetIndex<Element>?
    /// Insert a member into the set.
    public mutating func insert(member: Element)
    /// Remove the member from the set and return it if it was present.
    public mutating func remove(member: Element) -> Element?
    /// Remove the member referenced by the given index.
    public mutating func removeAtIndex(index: SetIndex<Element>) -> Element
    /// Erase all the elements.  If `keepCapacity` is `true`, `capacity`
    /// will not decrease.
    public mutating func removeAll(keepCapacity keepCapacity: Bool = default)
    /// Remove a member from the set and return it.
    ///
    /// - Requires: `count > 0`.
    public mutating func removeFirst() -> Element
    /// The number of members in the set.
    ///
    /// - Complexity: O(1).
    public var count: Int { get }
    public subscript (position: SetIndex<Element>) -> Element { get }
    /// Return a *generator* over the members.
    ///
    /// - Complexity: O(1).
    public func generate() -> SetGenerator<Element>
    public init(arrayLiteral elements: Element...)
    /// Create an empty `Set`.
    public init()
    /// Create a `Set` from a finite sequence of items.
    public init<S : SequenceType where S.Generator.Element == Element>(_ sequence: S)
    /// Returns true if the set is a subset of a finite sequence as a `Set`.
    @warn_unused_result
    public func isSubsetOf<S : SequenceType where S.Generator.Element == Element>(sequence: S) -> Bool
    /// Returns true if the set is a subset of a finite sequence as a `Set`
    /// but not equal.
    @warn_unused_result
    public func isStrictSubsetOf<S : SequenceType where S.Generator.Element == Element>(sequence: S) -> Bool
    /// Returns true if the set is a superset of a finite sequence as a `Set`.
    @warn_unused_result
    public func isSupersetOf<S : SequenceType where S.Generator.Element == Element>(sequence: S) -> Bool
    /// Returns true if the set is a superset of a finite sequence as a `Set`
    /// but not equal.
    @warn_unused_result
    public func isStrictSupersetOf<S : SequenceType where S.Generator.Element == Element>(sequence: S) -> Bool
    /// Returns true if no members in the set are in a finite sequence as a `Set`.
    @warn_unused_result
    public func isDisjointWith<S : SequenceType where S.Generator.Element == Element>(sequence: S) -> Bool
    /// Return a new `Set` with items in both this set and a finite sequence.
    @warn_unused_result
    public func union<S : SequenceType where S.Generator.Element == Element>(sequence: S) -> Set<Element>
    /// Insert elements of a finite sequence into this `Set`.
    public mutating func unionInPlace<S : SequenceType where S.Generator.Element == Element>(sequence: S)
    /// Return a new set with elements in this set that do not occur
    /// in a finite sequence.
    @warn_unused_result
    public func subtract<S : SequenceType where S.Generator.Element == Element>(sequence: S) -> Set<Element>
    /// Remove all members in the set that occur in a finite sequence.
    public mutating func subtractInPlace<S : SequenceType where S.Generator.Element == Element>(sequence: S)
    /// Return a new set with elements common to this set and a finite sequence.
    @warn_unused_result
    public func intersect<S : SequenceType where S.Generator.Element == Element>(sequence: S) -> Set<Element>
    /// Remove any members of this set that aren't also in a finite sequence.
    public mutating func intersectInPlace<S : SequenceType where S.Generator.Element == Element>(sequence: S)
    /// Return a new set with elements that are either in the set or a finite
    /// sequence but do not occur in both.
    @warn_unused_result
    public func exclusiveOr<S : SequenceType where S.Generator.Element == Element>(sequence: S) -> Set<Element>
    /// For each element of a finite sequence, remove it from the set if it is a
    /// common element, otherwise add it to the set. Repeated elements of the
    /// sequence will be ignored.
    public mutating func exclusiveOrInPlace<S : SequenceType where S.Generator.Element == Element>(sequence: S)
    public var hashValue: Int { get }
    /// `true` if the set is empty.
    public var isEmpty: Bool { get }
    /// The first element obtained when iterating, or `nil` if `self` is
    /// empty.  Equivalent to `self.generate().next()`.
    public var first: Element? { get }
}

extension Set : CustomStringConvertible, CustomDebugStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
}

extension Set : _Reflectable {
}

extension Set {
    /// If `!self.isEmpty`, return the first key-value pair in the sequence of
    /// elements, otherwise return `nil`.
    ///
    /// - Complexity: Amortized O(1)
    public mutating func popFirst() -> Element?
}



/// A generalized set whose distinct elements are not necessarily
/// disjoint.
///
/// In a model of `SetAlgebraType`, some elements may subsume other
/// elements, where
///
/// > `a` **subsumes** `b` iff `([a] as Self).isSupersetOf([b])`
///
/// In many models of `SetAlgebraType` such as `Set<T>`, `a`
/// *subsumes* `b` if and only if `a == b`, but that is not always the
/// case.  For example, option sets typically do not satisfy that
/// property.
///
/// Two elements are **disjoint** when neither one *subsumes* the other.
///
/// - SeeAlso: `OptionSetType`.
///
/// - Axioms, where `S` conforms to `SetAlgebraType`, `x` and `y` are
///   of type `S`, and `e` is of type `S.Element`:
///
///   - `S() == []`
///   - `x.intersect(x) == x`
///   - `x.intersect([]) == []`
///   - `x.union(x) == x`
///   - `x.union([]) == x`
///   - `x.contains(e)` implies `x.union(y).contains(e)`
///   - `x.union(y).contains(e)` implies `x.contains(e) || y.contains(e)`
///   - `x.contains(e) && y.contains(e)` iff `x.intersect(y).contains(e)`
///   - `x.isSubsetOf(y)` iff `y.isSupersetOf(x)`
///   - `x.isStrictSupersetOf(y)` iff `x.isSupersetOf(y) && x != y`
///   - `x.isStrictSubsetOf(y)` iff `x.isSubsetOf(y) && x != y`
public protocol SetAlgebraType : Equatable, ArrayLiteralConvertible {
    /// A type for which `Self` provides a containment test.
    typealias Element
    /// Creates an empty set.
    ///
    /// - Equivalent to `[] as Self`
    public init()
    /// Returns `true` if `self` contains `member`.
    ///
    /// - Equivalent to `self.intersect([member]) == [member]`
    @warn_unused_result
    public func contains(member: Self.Element) -> Bool
    /// Returns the set of elements contained in `self`, in `other`, or in
    /// both `self` and `other`.
    @warn_unused_result
    public func union(other: Self) -> Self
    /// Returns the set of elements contained in both `self` and `other`.
    @warn_unused_result
    public func intersect(other: Self) -> Self
    /// Returns the set of elements contained in `self` or in `other`,
    /// but not in both `self` and `other`.
    @warn_unused_result
    public func exclusiveOr(other: Self) -> Self
    /// If `member` is not already contained in `self`, inserts it.
    ///
    /// - Equivalent to `self.unionInPlace([member])`
    /// - Postcondition: `self.contains(member)`
    public mutating func insert(member: Self.Element)
    /// If `member` is contained in `self`, removes and returns it.
    /// Otherwise, removes all elements subsumed by `member` and returns
    /// `nil`.
    ///
    /// - Postcondition: `self.intersect([member]).isEmpty`
    public mutating func remove(member: Self.Element) -> Self.Element?
    /// Insert all elements of `other` into `self`.
    ///
    /// - Equivalent to replacing `self` with `self.union(other)`.
    /// - Postcondition: `self.isSupersetOf(other)`
    public mutating func unionInPlace(other: Self)
    /// Removes all elements of `self` that are not also present in
    /// `other`.
    ///
    /// - Equivalent to replacing `self` with `self.intersect(other)`
    /// - Postcondition: `self.isSubsetOf(other)`
    public mutating func intersectInPlace(other: Self)
    /// Replaces `self` with a set containing all elements contained in
    /// either `self` or `other`, but not both.
    ///
    /// - Equivalent to replacing `self` with `self.exclusiveOr(other)`
    public mutating func exclusiveOrInPlace(other: Self)
    /// Return true iff `self.intersect(other).isEmpty`.
    @warn_unused_result
    public func subtract(other: Self) -> Self
    /// Return true iff every element of `self` is contained in `other`.
    @warn_unused_result
    public func isSubsetOf(other: Self) -> Bool
    /// Return true iff `self.intersect(other).isEmpty`.
    @warn_unused_result
    public func isDisjointWith(other: Self) -> Bool
    /// Return true iff every element of `other` is contained in `self`.
    @warn_unused_result
    public func isSupersetOf(other: Self) -> Bool
    /// Return true iff `self.contains(e)` is `false` for all `e`.
    public var isEmpty: Bool { get }
    /// Creates the set containing all elements of `sequence`.
    public init<S : SequenceType where S.Generator.Element == Element>(_ sequence: S)
    /// Removes all elements of `other` from `self`.
    ///
    /// - Equivalent to replacing `self` with `self.subtract(other)`.
    public mutating func subtractInPlace(other: Self)
    /// Returns `true` iff `a` subsumes `b`.
    ///
    /// - Equivalent to `([a] as Self).isSupersetOf([b])`
    @warn_unused_result
    public static func element(a: Self.Element, subsumes b: Self.Element) -> Bool
    /// Returns `true` iff `a` is disjoint with `b`.
    ///
    /// Two elements are disjoint when neither one subsumes the other.
    ///
    /// - SeeAlso: `Self.element(_, subsumes:_)`
    @warn_unused_result
    public static func element(a: Self.Element, isDisjointWith b: Self.Element) -> Bool
}

extension SetAlgebraType {
    /// Creates the set containing all elements of `sequence`.
    public convenience init<S : SequenceType where S.Generator.Element == Element>(_ sequence: S)
    /// Creates a set containing all elements of the given `arrayLiteral`.
    ///
    /// This initializer allows an array literal containing
    /// `Self.Element` to represent an instance of the set, wherever it
    /// is implied by the type context.
    public convenience init(arrayLiteral: Self.Element...)
    /// Removes all elements of `other` from `self`.
    ///
    /// - Equivalent to replacing `self` with `self.subtract(other)`.
    public mutating func subtractInPlace(other: Self)
    /// Returns true iff every element of `self` is contained in `other`.
    @warn_unused_result
    public func isSubsetOf(other: Self) -> Bool
    /// Returns true iff every element of `other` is contained in `self`.
    @warn_unused_result
    public func isSupersetOf(other: Self) -> Bool
    /// Returns true iff `self.intersect(other).isEmpty`.
    @warn_unused_result
    public func isDisjointWith(other: Self) -> Bool
    /// Returns true iff `self.intersect(other).isEmpty`.
    @warn_unused_result
    public func subtract(other: Self) -> Self
    /// Returns true iff `self.contains(e)` is `false` for all `e`.
    public var isEmpty: Bool { get }
    /// Returns true iff every element of `other` is contained in `self`
    /// and `self` contains an element that is not contained in `other`.
    @warn_unused_result
    public func isStrictSupersetOf(other: Self) -> Bool
    /// Return true iff every element of `self` is contained in `other`
    /// and `other` contains an element that is not contained in `self`.
    @warn_unused_result
    public func isStrictSubsetOf(other: Self) -> Bool
    /// Returns `true` iff `a` subsumes `b`.
    ///
    /// - Equivalent to `([a] as Self).isSupersetOf([b])`
    @warn_unused_result
    public static func element(a: Self.Element, subsumes b: Self.Element) -> Bool
    /// Returns `true` iff `a` is disjoint with `b`.
    ///
    /// Two elements are disjoint when neither one subsumes the other.
    ///
    /// - SeeAlso: `Self.element(_, subsumes:_)`
    @warn_unused_result
    public static func element(a: Self.Element, isDisjointWith b: Self.Element) -> Bool
}

/// A generator over the members of a `Set<Element>`.
public struct SetGenerator<Element : Hashable> : GeneratorType {
    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: No preceding call to `self.next()` has returned `nil`.
    public mutating func next() -> Element?
}

/// Used to access the members in an instance of `Set<Element>`.
public struct SetIndex<Element : Hashable> : ForwardIndexType, Comparable {
    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    public func successor() -> SetIndex<Element>
}

/// A set of common requirements for Swift's signed integer types.
public protocol SignedIntegerType : _SignedIntegerType, IntegerType {
    /// Represent this number using Swift's widest native signed integer
    /// type.
    public func toIntMax() -> IntMax
    /// Convert from Swift's widest signed integer type, trapping on
    /// overflow.
    public init(_: IntMax)
}

/// Instances of conforming types can be subtracted, arithmetically
/// negated, and initialized from `0`.
///
/// Axioms:
///
/// - `x - 0 == x`
/// - `-x == 0 - x`
/// - `-(-x) == x`
public protocol SignedNumberType : Comparable, IntegerLiteralConvertible {
    /// Return the result of negating `x`.
    @warn_unused_result
    prefix public func -(x: Self) -> Self
    /// Return the difference between `lhs` and `rhs`.
    @warn_unused_result
    public func -(lhs: Self, rhs: Self) -> Self
}

/// A view into a sub-sequence of elements of another collection.
///
/// A `Slice` instance stores the base collection, the start and end indices of
/// the view.  It does not copy the elements from the collection into separate
/// storage. Thus, creating a slice has `O(1)` complexity.
///
/// A `Slice` instance inherits the value or reference semantics of the base
/// collection.  That is, if a `Slice` instance is wrapped around a mutable
/// colection that has value semantics (for example, `Array`), mutating the
/// original collection would not affect the copy stored inside of the slice.
///
/// An element of a slice is located under the same index in the slice and in
/// the base collection, as long as neither the collection or the slice were
/// mutated.  Thus, indices of a slice can be used interchangibly with indices
/// of the base collection.
///
/// - Warning: Long-term storage of `Slice` instances is discouraged.
///
///   Because a `Slice` presents a *view* onto the storage of some larger
///   collection even after the original collection goes out of scope, storing
///   the slice may prolong the lifetime of elements that are no longer
///   accessible, which can manifest as apparent memory and object leakage.  To
///   prevent this effect, use slices only for transient computation.
public struct Slice<Base : Indexable> : CollectionType {
    public typealias Index = Base.Index
    public let startIndex: Base.Index
    public let endIndex: Base.Index
    public subscript (index: Base.Index) -> Base._Element { get }
    public subscript (bounds: Range<Base.Index>) -> Slice<Base> { get }
    /// Create a view into collection `base` that allows access within `bounds`.
    ///
    /// - Complexity: O(1).
    public init(base: Base, bounds: Range<Base.Index>)
}

/// An simple string designed to represent text that is "knowable at
/// compile-time".
///
/// Logically speaking, each instance looks something like this:
///
///      enum StaticString {
///         case ASCII(start: UnsafePointer<UInt8>, length: Int)
///         case UTF8(start: UnsafePointer<UInt8>, length: Int)
///         case Scalar(UnicodeScalar)
///      }
public struct StaticString : UnicodeScalarLiteralConvertible, ExtendedGraphemeClusterLiteralConvertible, StringLiteralConvertible, CustomStringConvertible, CustomDebugStringConvertible, _Reflectable {
    /// A pointer to the beginning of UTF-8 code units.
    ///
    /// - Requires: `self` stores a pointer to either ASCII or UTF-8 code
    ///   units.
    public var utf8Start: UnsafePointer<UInt8> { get }
    /// The stored Unicode scalar value.
    ///
    /// - Requires: `self` stores a single Unicode scalar value.
    public var unicodeScalar: UnicodeScalar { get }
    /// If `self` stores a pointer to ASCII or UTF-8 code units, the
    /// length in bytes of that data.
    ///
    /// If `self` stores a single Unicode scalar value, the value of
    /// `byteSize` is unspecified.
    public var byteSize: Int { get }
    /// `true` iff `self` stores a pointer to ASCII or UTF-8 code units.
    public var hasPointerRepresentation: Bool { get }
    /// `true` if `self` stores a pointer to ASCII code units.
    ///
    /// If `self` stores a single Unicode scalar value, the value of
    /// `isASCII` is unspecified.
    public var isASCII: Bool { get }
    /// Invoke `body` with a buffer containing the UTF-8 code units of
    /// `self`.
    ///
    /// This method works regardless of what `self` stores.
    public func withUTF8Buffer<R>(@noescape body: (UnsafeBufferPointer<UInt8>) -> R) -> R
    /// Return a `String` representing the same sequence of Unicode
    /// scalar values as `self` does.
    public var stringValue: String { get }
    /// Create an empty instance.
    public init()
    public init(_builtinUnicodeScalarLiteral value: Builtin.Int32)
    /// Create an instance initialized to `value`.
    public init(unicodeScalarLiteral value: StaticString)
    public init(_builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer, byteSize: Builtin.Word, isASCII: Builtin.Int1)
    /// Create an instance initialized to `value`.
    public init(extendedGraphemeClusterLiteral value: StaticString)
    public init(_builtinStringLiteral start: Builtin.RawPointer, byteSize: Builtin.Word, isASCII: Builtin.Int1)
    /// Create an instance initialized to `value`.
    public init(stringLiteral value: StaticString)
    /// A textual representation of `self`.
    public var description: String { get }
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
}

/// A source of text streaming operations.  `Streamable` instances can
/// be written to any *output stream*.
///
/// For example: `String`, `Character`, `UnicodeScalar`.
public protocol Streamable {
    /// Write a textual representation of `self` into `target`.
    public func writeTo<Target : OutputStreamType>(inout target: Target)
}

/// A `SequenceType` of values formed by striding over a closed interval.
public struct StrideThrough<Element : Strideable> : SequenceType {
    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    public func generate() -> StrideThroughGenerator<Element>
}

extension StrideThrough : _Reflectable {
}

/// A GeneratorType for `StrideThrough<Element>`.
public struct StrideThroughGenerator<Element : Strideable> : GeneratorType {
    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    public mutating func next() -> Element?
}

/// A `SequenceType` of values formed by striding over a half-open interval.
public struct StrideTo<Element : Strideable> : SequenceType {
    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    public func generate() -> StrideToGenerator<Element>
}

extension StrideTo : _Reflectable {
}

/// A GeneratorType for `StrideTo<Element>`.
public struct StrideToGenerator<Element : Strideable> : GeneratorType {
    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    public mutating func next() -> Element?
}

/// Conforming types are notionally continuous, one-dimensional
/// values that can be offset and measured.
public protocol Strideable : Comparable, _Strideable {
    /// A type that can represent the distance between two values of `Self`.
    typealias Stride : SignedNumberType
    /// Returns a stride `x` such that `self.advancedBy(x)` approximates
    /// `other`.
    ///
    /// - Complexity: O(1).
    ///
    /// - SeeAlso: `RandomAccessIndexType`'s `distanceTo`, which provides a
    ///   stronger semantic guarantee.
    @warn_unused_result
    public func distanceTo(other: Self) -> Self.Stride
    /// Returns a `Self` `x` such that `self.distanceTo(x)` approximates
    /// `n`.
    ///
    /// - Complexity: O(1).
    ///
    /// - SeeAlso: `RandomAccessIndexType`'s `advancedBy`, which
    ///   provides a stronger semantic guarantee.
    @warn_unused_result
    public func advancedBy(n: Self.Stride) -> Self
}

extension Strideable {
    /// Return the sequence of values (`self`, `self + stride`, `self +
    /// stride + stride`, ... *last*) where *last* is the last value in
    /// the progression that is less than `end`.
    @warn_unused_result
    public func stride(to end: Self, by stride: Self.Stride) -> StrideTo<Self>
}

extension Strideable {
    /// Return the sequence of values (`start`, `start + stride`, `start +
    /// stride + stride`, ... *last*) where *last* is the last value in
    /// the progression less than or equal to `end`.
    ///
    /// - Note: There is no guarantee that `end` is an element of the sequence.
    @warn_unused_result
    public func stride(through end: Self, by stride: Self.Stride) -> StrideThrough<Self>
}

/// An arbitrary Unicode string value.
///
/// Unicode-Correct
/// ===============
///
/// Swift strings are designed to be Unicode-correct.  In particular,
/// the APIs make it easy to write code that works correctly, and does
/// not surprise end-users, regardless of where you venture in the
/// Unicode character space.  For example, the `==` operator checks
/// for [Unicode canonical
/// equivalence](http://www.unicode.org/glossary/#deterministic_comparison),
/// so two different representations of the same string will always
/// compare equal.
///
/// Locale-Insensitive
/// ==================
///
/// The fundamental operations on Swift strings are not sensitive to
/// locale settings.  That's because, for example, the validity of a
/// `Dictionary<String, T>` in a running program depends on a given
/// string comparison having a single, stable result.  Therefore,
/// Swift always uses the default,
/// un-[tailored](http://www.unicode.org/glossary/#tailorable) Unicode
/// algorithms for basic string operations.
///
/// Importing `Foundation` endows swift strings with the full power of
/// the `NSString` API, which allows you to choose more complex
/// locale-sensitive operations explicitly.
///
/// Value Semantics
/// ===============
///
/// Each string variable, `let` binding, or stored property has an
/// independent value, so mutations to the string are not observable
/// through its copies:
///
///     var a = "foo"
///     var b = a
///     b.appendContentsOf("bar")
///     print("a=\(a), b=\(b)")     // a=foo, b=foobar
///
/// Strings use Copy-on-Write so that their data is only copied
/// lazily, upon mutation, when more than one string instance is using
/// the same buffer.  Therefore, the first in any sequence of mutating
/// operations may cost `O(N)` time and space, where `N` is the length
/// of the string's (unspecified) underlying representation.
///
/// Views
/// =====
///
/// `String` is not itself a collection of anything.  Instead, it has
/// properties that present the string's contents as meaningful
/// collections:
///
///   - `characters`: a collection of `Character` ([extended grapheme
///     cluster](http://www.unicode.org/glossary/#extended_grapheme_cluster))
///     elements, a unit of text that is meaningful to most humans.
///
///   - `unicodeScalars`: a collection of `UnicodeScalar` ([Unicode
///     scalar
///     values](http://www.unicode.org/glossary/#unicode_scalar_value))
///     the 21-bit codes that are the basic unit of Unicode.  These
///     values are equivalent to UTF-32 code units.
///
///   - `utf16`: a collection of `UTF16.CodeUnit`, the 16-bit
///     elements of the string's UTF-16 encoding.
///
///   - `utf8`: a collection of `UTF8.CodeUnit`, the 8-bit
///     elements of the string's UTF-8 encoding.
///
/// Growth and Capacity
/// ===================
///
/// When a string's contiguous storage fills up, new storage must be
/// allocated and characters must be moved to the new storage.
/// `String` uses an exponential growth strategy that makes `append` a
/// constant time operation *when amortized over many invocations*.
///
/// Objective-C Bridge
/// ==================
///
/// `String` is bridged to Objective-C as `NSString`, and a `String`
/// that originated in Objective-C may store its characters in an
/// `NSString`.  Since any arbitrary subclass of `NSSString` can
/// become a `String`, there are no guarantees about representation or
/// efficiency in this case.  Since `NSString` is immutable, it is
/// just as though the storage was shared by some copy: the first in
/// any sequence of mutating operations causes elements to be copied
/// into unique, contiguous storage which may cost `O(N)` time and
/// space, where `N` is the length of the string representation (or
/// more, if the underlying `NSString` is has unusual performance
/// characteristics).
public struct String {
    /// An empty `String`.
    public init()
}

extension String {
    public typealias Index = String.CharacterView.Index
    /// The position of the first `Character` in `self.characters` if
    /// `self` is non-empty; identical to `endIndex` otherwise.
    public var startIndex: Index { get }
    /// The "past the end" position in `self.characters`.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    public var endIndex: Index { get }
    public subscript (i: Index) -> Character { get }
}

extension String {
    /// A `String`'s collection of `Character`s ([extended grapheme
    /// clusters](http://www.unicode.org/glossary/#extended_grapheme_cluster))
    /// elements.
    public struct CharacterView {
        /// Create a view of the `Character`s in `text`.
        public init(_ text: String)
    }
    /// A collection of `Characters` representing the `String`'s
    /// [extended grapheme
    /// clusters](http://www.unicode.org/glossary/#extended_grapheme_cluster).
    public var characters: String.CharacterView { get }
    /// Efficiently mutate `self` by applying `body` to its `characters`.
    ///
    /// - Warning: Do not rely on anything about `self` (the `String`
    ///   that is the target of this method) during the execution of
    ///   `body`: it may not appear to have its correct value.  Instead,
    ///   use only the `String.CharacterView` argument to `body`.
    public mutating func withMutableCharacters<R>(body: (inout String.CharacterView) -> R) -> R
    /// Construct the `String` corresponding to the given sequence of
    /// Unicode scalars.
    public init(_ characters: String.CharacterView)
}

extension String {
    /// A collection of [Unicode scalar values](http://www.unicode.org/glossary/#unicode_scalar_value) that
    /// encode a `String` .
    public struct UnicodeScalarView : CollectionType, _Reflectable, CustomStringConvertible, CustomDebugStringConvertible {
        /// A position in a `String.UnicodeScalarView`.
        public struct Index : BidirectionalIndexType, Comparable {
            /// Returns the next consecutive value after `self`.
            ///
            /// - Requires: The next value is representable.
            @warn_unused_result
            public func successor() -> String.UnicodeScalarView.Index
            /// Returns the previous consecutive value before `self`.
            ///
            /// - Requires: The previous value is representable.
            @warn_unused_result
            public func predecessor() -> String.UnicodeScalarView.Index
        }
        /// The position of the first `UnicodeScalar` if the `String` is
        /// non-empty; identical to `endIndex` otherwise.
        public var startIndex: String.UnicodeScalarView.Index { get }
        /// The "past the end" position.
        ///
        /// `endIndex` is not a valid argument to `subscript`, and is always
        /// reachable from `startIndex` by zero or more applications of
        /// `successor()`.
        public var endIndex: String.UnicodeScalarView.Index { get }
        public subscript (position: String.UnicodeScalarView.Index) -> UnicodeScalar { get }
        public subscript (r: Range<String.UnicodeScalarView.Index>) -> String.UnicodeScalarView { get }
        /// A type whose instances can produce the elements of this
        /// sequence, in order.
        public struct Generator : GeneratorType {
            /// Advance to the next element and return it, or `nil` if no next
            /// element exists.
            ///
            /// - Requires: No preceding call to `self.next()` has returned
            ///   `nil`.
            public mutating func next() -> UnicodeScalar?
        }
        /// Return a *generator* over the `UnicodeScalar`s that comprise
        /// this *sequence*.
        ///
        /// - Complexity: O(1).
        @warn_unused_result
        public func generate() -> String.UnicodeScalarView.Generator
        public var description: String { get }
        public var debugDescription: String { get }
    }
    /// Construct the `String` corresponding to the given sequence of
    /// Unicode scalars.
    public init(_ unicodeScalars: String.UnicodeScalarView)
    /// The index type for subscripting a `String`'s `.unicodeScalars`
    /// view.
    public typealias UnicodeScalarIndex = String.UnicodeScalarView.Index
}

extension String {
    /// A collection of UTF-16 code units that encodes a `String` value.
    public struct UTF16View : CollectionType, _Reflectable, CustomStringConvertible, CustomDebugStringConvertible {
        public struct Index {
        }
        /// The position of the first code unit if the `String` is
        /// non-empty; identical to `endIndex` otherwise.
        public var startIndex: String.UTF16View.Index { get }
        /// The "past the end" position.
        ///
        /// `endIndex` is not a valid argument to `subscript`, and is always
        /// reachable from `startIndex` by zero or more applications of
        /// `successor()`.
        public var endIndex: String.UTF16View.Index { get }
        public subscript (i: String.UTF16View.Index) -> CodeUnit { get }
        public subscript (subRange: Range<String.UTF16View.Index>) -> String.UTF16View { get }
        public var description: String { get }
        public var debugDescription: String { get }
    }
    /// A UTF-16 encoding of `self`.
    public var utf16: String.UTF16View { get }
    /// Construct the `String` corresponding to the given sequence of
    /// UTF-16 code units.  If `utf16` contains unpaired surrogates, the
    /// result is `nil`.
    public init?(_ utf16: String.UTF16View)
    /// The index type for subscripting a `String`'s `utf16` view.
    public typealias UTF16Index = String.UTF16View.Index
}

extension String {
    /// A collection of UTF-8 code units that encodes a `String` value.
    public struct UTF8View : CollectionType, _Reflectable, CustomStringConvertible, CustomDebugStringConvertible {
        /// A position in a `String.UTF8View`.
        public struct Index : ForwardIndexType {
            /// Returns the next consecutive value after `self`.
            ///
            /// - Requires: The next value is representable.
            @warn_unused_result
            public func successor() -> String.UTF8View.Index
        }
        /// The position of the first code unit if the `String` is
        /// non-empty; identical to `endIndex` otherwise.
        public var startIndex: String.UTF8View.Index { get }
        /// The "past the end" position.
        ///
        /// `endIndex` is not a valid argument to `subscript`, and is always
        /// reachable from `startIndex` by zero or more applications of
        /// `successor()`.
        public var endIndex: String.UTF8View.Index { get }
        public subscript (position: String.UTF8View.Index) -> CodeUnit { get }
        public subscript (subRange: Range<String.UTF8View.Index>) -> String.UTF8View { get }
        public var description: String { get }
        public var debugDescription: String { get }
    }
    /// A UTF-8 encoding of `self`.
    public var utf8: String.UTF8View { get }
    /// A contiguously-stored nul-terminated UTF-8 representation of
    /// `self`.
    ///
    /// To access the underlying memory, invoke
    /// `withUnsafeBufferPointer` on the `ContiguousArray`.
    public var nulTerminatedUTF8: ContiguousArray<CodeUnit> { get }
    /// Construct the `String` corresponding to the given sequence of
    /// UTF-8 code units.  If `utf8` contains unpaired surrogates, the
    /// result is `nil`.
    public init?(_ utf8: String.UTF8View)
    /// The index type for subscripting a `String`'s `.utf8` view.
    public typealias UTF8Index = String.UTF8View.Index
}

extension String {
    /// Creates a new `String` by copying the nul-terminated UTF-8 data
    /// referenced by a `CString`.
    ///
    /// Returns `nil` if the `CString` is `NULL` or if it contains ill-formed
    /// UTF-8 code unit sequences.
    @warn_unused_result
    public static func fromCString(cs: UnsafePointer<CChar>) -> String?
    /// Creates a new `String` by copying the nul-terminated UTF-8 data
    /// referenced by a `CString`.
    ///
    /// Returns `nil` if the `CString` is `NULL`.  If `CString` contains
    /// ill-formed UTF-8 code unit sequences, replaces them with replacement
    /// characters (U+FFFD).
    @warn_unused_result
    public static func fromCStringRepairingIllFormedUTF8(cs: UnsafePointer<CChar>) -> (String?, hadError: Bool)
}

extension String {
    /// Construct an instance containing just the given `Character`.
    public init(_ c: Character)
}

extension String {
    /// Invoke `f` on the contents of this string, represented as
    /// a nul-terminated array of char, ensuring that the array's
    /// lifetime extends through the execution of `f`.
    public func withCString<Result>(@noescape f: UnsafePointer<Int8> throws -> Result) rethrows -> Result
}

extension String : _Reflectable {
}

extension String : OutputStreamType {
    /// Append `other` to this stream.
    public mutating func write(other: String)
}

extension String : Streamable {
    /// Write a textual representation of `self` into `target`.
    public func writeTo<Target : OutputStreamType>(inout target: Target)
}



extension String {
    public init(_builtinUnicodeScalarLiteral value: Builtin.Int32)
}

extension String : UnicodeScalarLiteralConvertible {
    /// Create an instance initialized to `value`.
    public init(unicodeScalarLiteral value: String)
}

extension String {
    public init(_builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer, byteSize: Builtin.Word, isASCII: Builtin.Int1)
}

extension String : ExtendedGraphemeClusterLiteralConvertible {
    /// Create an instance initialized to `value`.
    public init(extendedGraphemeClusterLiteral value: String)
}

extension String {
    public init(_builtinUTF16StringLiteral start: Builtin.RawPointer, numberOfCodeUnits: Builtin.Word)
}

extension String {
    public init(_builtinStringLiteral start: Builtin.RawPointer, byteSize: Builtin.Word, isASCII: Builtin.Int1)
}

extension String : StringLiteralConvertible {
    /// Create an instance initialized to `value`.
    public init(stringLiteral value: String)
}

extension String : CustomDebugStringConvertible {
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
}



extension String : Equatable {
}

extension String : Comparable {
}



extension String {
    /// Append the elements of `other` to `self`.
    public mutating func appendContentsOf(other: String)
    /// Append `x` to `self`.
    ///
    /// - Complexity: Amortized O(1).
    public mutating func append(x: UnicodeScalar)
}

extension String : Hashable {
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
}



extension String {
    public subscript (subRange: Range<Index>) -> String { get }
}

extension String {
    public mutating func reserveCapacity(n: Int)
    public mutating func append(c: Character)
    public mutating func appendContentsOf<S : SequenceType where S.Generator.Element == Character>(newElements: S)
    /// Create an instance containing `characters`.
    public init<S : SequenceType where S.Generator.Element == Character>(_ characters: S)
}



extension String {
    /// Replace the given `subRange` of elements with `newElements`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`subRange.count`) if `subRange.endIndex
    ///   == self.endIndex` and `newElements.isEmpty`, O(N) otherwise.
    public mutating func replaceRange<C : CollectionType where C.Generator.Element == Character>(subRange: Range<Index>, with newElements: C)
    /// Replace the given `subRange` of elements with `newElements`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`subRange.count`) if `subRange.endIndex
    ///   == self.endIndex` and `newElements.isEmpty`, O(N) otherwise.
    public mutating func replaceRange(subRange: Range<Index>, with newElements: String)
    /// Insert `newElement` at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count`).
    public mutating func insert(newElement: Character, atIndex i: Index)
    /// Insert `newElements` at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count + newElements.count`).
    public mutating func insertContentsOf<S : CollectionType where S.Generator.Element == Character>(newElements: S, at i: Index)
    /// Remove and return the element at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count`).
    public mutating func removeAtIndex(i: Index) -> Character
    /// Remove the indicated `subRange` of characters.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count`).
    public mutating func removeRange(subRange: Range<Index>)
    /// Remove all characters.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - parameter keepCapacity: If `true`, prevents the release of
    ///   allocated storage, which can be a useful optimization
    ///   when `self` is going to be grown again.
    public mutating func removeAll(keepCapacity keepCapacity: Bool = default)
}

extension String {
    public var lowercaseString: String { get }
    public var uppercaseString: String { get }
}





extension String : StringInterpolationConvertible {
    /// Create an instance by concatenating the elements of `strings`.
    public init(stringInterpolation strings: String...)
    /// Create an instance containing `expr`'s `print` representation.
    public init<T>(stringInterpolationSegment expr: T)
    public init(stringInterpolationSegment expr: String)
    public init(stringInterpolationSegment expr: Character)
    public init(stringInterpolationSegment expr: UnicodeScalar)
    public init(stringInterpolationSegment expr: Bool)
    public init(stringInterpolationSegment expr: Float32)
    public init(stringInterpolationSegment expr: Float64)
    public init(stringInterpolationSegment expr: UInt8)
    public init(stringInterpolationSegment expr: Int8)
    public init(stringInterpolationSegment expr: UInt16)
    public init(stringInterpolationSegment expr: Int16)
    public init(stringInterpolationSegment expr: UInt32)
    public init(stringInterpolationSegment expr: Int32)
    public init(stringInterpolationSegment expr: UInt64)
    public init(stringInterpolationSegment expr: Int64)
    public init(stringInterpolationSegment expr: UInt)
    public init(stringInterpolationSegment expr: Int)
}

extension String {
    /// Construct an instance that is the concatenation of `count` copies
    /// of `repeatedValue`.
    public init(count: Int, repeatedValue c: Character)
    /// Construct an instance that is the concatenation of `count` copies
    /// of `Character(repeatedValue)`.
    public init(count: Int, repeatedValue c: UnicodeScalar)
    /// `true` iff `self` contains no characters.
    public var isEmpty: Bool { get }
}



extension String {
    /// Returns `true` iff `self` begins with `prefix`.
    public func hasPrefix(prefix: String) -> Bool
    /// Returns `true` iff `self` ends with `suffix`.
    public func hasSuffix(suffix: String) -> Bool
}

extension String {
    /// Create an instance representing `v` in base 10.
    public init<T : _SignedIntegerType>(_ v: T)
    /// Create an instance representing `v` in base 10.
    public init<T : UnsignedIntegerType>(_ v: T)
    /// Create an instance representing `v` in the given `radix` (base).
    ///
    /// Numerals greater than 9 are represented as roman letters,
    /// starting with `a` if `uppercase` is `false` or `A` otherwise.
    public init<T : _SignedIntegerType>(_ v: T, radix: Int, uppercase: Bool = default)
    /// Create an instance representing `v` in the given `radix` (base).
    ///
    /// Numerals greater than 9 are represented as roman letters,
    /// starting with `a` if `uppercase` is `false` or `A` otherwise.
    public init<T : UnsignedIntegerType>(_ v: T, radix: Int, uppercase: Bool = default)
}





extension String {
    /// The value of `self` as a collection of [Unicode scalar values](http://www.unicode.org/glossary/#unicode_scalar_value).
    public var unicodeScalars: String.UnicodeScalarView
}







extension String {
}

extension String : MirrorPathType {
}

extension String {
    /// Initialize `self` with the textual representation of `instance`.
    ///
    /// * If `T` conforms to `Streamable`, the result is obtained by
    ///   calling `instance.writeTo(s)` on an empty string s.
    /// * Otherwise, if `T` conforms to `CustomStringConvertible`, the
    ///   result is `instance`'s `description`
    /// * Otherwise, if `T` conforms to `CustomDebugStringConvertible`,
    ///   the result is `instance`'s `debugDescription`
    /// * Otherwise, an unspecified result is supplied automatically by
    ///   the Swift standard library.
    ///
    /// - SeeAlso: `String.init<T>(reflecting: T)`
    public init<T>(_ instance: T)
    /// Initialize `self` with a detailed textual representation of
    /// `subject`, suitable for debugging.
    ///
    /// * If `T` conforms to `CustomDebugStringConvertible`, the result
    ///   is `subject`'s `debugDescription`.
    ///
    /// * Otherwise, if `T` conforms to `CustomStringConvertible`, the result
    ///   is `subject`'s `description`.
    ///
    /// * Otherwise, if `T` conforms to `Streamable`, the result is
    ///   obtained by calling `subject.writeTo(s)` on an empty string s.
    ///
    /// * Otherwise, an unspecified result is supplied automatically by
    ///   the Swift standard library.
    ///
    /// - SeeAlso: `String.init<T>(T)`
    public init<T>(reflecting subject: T)
}

extension String.CharacterView : CollectionType {
    /// A character position.
    public struct Index : BidirectionalIndexType, Comparable, _Reflectable {
        /// Returns the next consecutive value after `self`.
        ///
        /// - Requires: The next value is representable.
        public func successor() -> String.CharacterView.Index
        /// Returns the previous consecutive value before `self`.
        ///
        /// - Requires: The previous value is representable.
        public func predecessor() -> String.CharacterView.Index
    }
    /// The position of the first `Character` if `self` is
    /// non-empty; identical to `endIndex` otherwise.
    public var startIndex: String.CharacterView.Index { get }
    /// The "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    public var endIndex: String.CharacterView.Index { get }
    public subscript (i: String.CharacterView.Index) -> Character { get }
}

extension String.CharacterView : RangeReplaceableCollectionType {
    /// Create an empty instance.
    public init()
    /// Replace the given `subRange` of elements with `newElements`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`subRange.count`) if `subRange.endIndex
    ///   == self.endIndex` and `newElements.isEmpty`, O(N) otherwise.
    public mutating func replaceRange<C : CollectionType where C.Generator.Element == Character>(subRange: Range<String.CharacterView.Index>, with newElements: C)
    /// Reserve enough space to store `n` ASCII characters.
    ///
    /// - Complexity: O(`n`).
    public mutating func reserveCapacity(n: Int)
    /// Append `c` to `self`.
    ///
    /// - Complexity: Amortized O(1).
    public mutating func append(c: Character)
    /// Append the elements of `newElements` to `self`.
    public mutating func appendContentsOf<S : SequenceType where S.Generator.Element == Character>(newElements: S)
    /// Create an instance containing `characters`.
    public init<S : SequenceType where S.Generator.Element == Character>(_ characters: S)
}

extension String.CharacterView {
    public subscript (subRange: Range<String.CharacterView.Index>) -> String.CharacterView { get }
}

extension String.UnicodeScalarView : RangeReplaceableCollectionType {
    /// Construct an empty instance.
    public init()
    /// Reserve enough space to store `n` ASCII characters.
    ///
    /// - Complexity: O(`n`).
    public mutating func reserveCapacity(n: Int)
    /// Append `x` to `self`.
    ///
    /// - Complexity: Amortized O(1).
    public mutating func append(x: UnicodeScalar)
    /// Append the elements of `newElements` to `self`.
    ///
    /// - Complexity: O(*length of result*).
    public mutating func appendContentsOf<S : SequenceType where S.Generator.Element == UnicodeScalar>(newElements: S)
    /// Replace the given `subRange` of elements with `newElements`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`subRange.count`) if `subRange.endIndex
    ///   == self.endIndex` and `newElements.isEmpty`, O(N) otherwise.
    public mutating func replaceRange<C : CollectionType where C.Generator.Element == UnicodeScalar>(subRange: Range<String.UnicodeScalarView.Index>, with newElements: C)
}

extension String.CharacterView.Index {
    /// Construct the position in `characters` that corresponds exactly to
    /// `unicodeScalarIndex`. If no such position exists, the result is `nil`.
    ///
    /// - Requires: `unicodeScalarIndex` is an element of
    ///   `characters.unicodeScalars.indices`.
    public init?(_ unicodeScalarIndex: UnicodeScalarIndex, within characters: String)
    /// Construct the position in `characters` that corresponds exactly to
    /// `utf16Index`. If no such position exists, the result is `nil`.
    ///
    /// - Requires: `utf16Index` is an element of
    ///   `characters.utf16.indices`.
    public init?(_ utf16Index: UTF16Index, within characters: String)
    /// Construct the position in `characters` that corresponds exactly to
    /// `utf8Index`. If no such position exists, the result is `nil`.
    ///
    /// - Requires: `utf8Index` is an element of
    ///   `characters.utf8.indices`.
    public init?(_ utf8Index: UTF8Index, within characters: String)
    /// Return the position in `utf8` that corresponds exactly
    /// to `self`.
    ///
    /// - Requires: `self` is an element of `String(utf8).indices`.
    @warn_unused_result
    public func samePositionIn(utf8: String.UTF8View) -> String.UTF8View.Index
    /// Return the position in `utf16` that corresponds exactly
    /// to `self`.
    ///
    /// - Requires: `self` is an element of `String(utf16).indices`.
    @warn_unused_result
    public func samePositionIn(utf16: String.UTF16View) -> String.UTF16View.Index
    /// Return the position in `unicodeScalars` that corresponds exactly
    /// to `self`.
    ///
    /// - Requires: `self` is an element of `String(unicodeScalars).indices`.
    @warn_unused_result
    public func samePositionIn(unicodeScalars: String.UnicodeScalarView) -> String.UnicodeScalarView.Index
}

extension String.UnicodeScalarView.Index {
    /// Construct the position in `unicodeScalars` that corresponds exactly to
    /// `utf16Index`. If no such position exists, the result is `nil`.
    ///
    /// - Requires: `utf16Index` is an element of
    ///   `String(unicodeScalars).utf16.indices`.
    public init?(_ utf16Index: UTF16Index, within unicodeScalars: String.UnicodeScalarView)
    /// Construct the position in `unicodeScalars` that corresponds exactly to
    /// `utf8Index`. If no such position exists, the result is `nil`.
    ///
    /// - Requires: `utf8Index` is an element of
    ///   `String(unicodeScalars).utf8.indices`.
    public init?(_ utf8Index: UTF8Index, within unicodeScalars: String.UnicodeScalarView)
    /// Construct the position in `unicodeScalars` that corresponds
    /// exactly to `characterIndex`.
    ///
    /// - Requires: `characterIndex` is an element of
    ///   `String(unicodeScalars).indices`.
    public init(_ characterIndex: Index, within unicodeScalars: String.UnicodeScalarView)
    /// Return the position in `utf8` that corresponds exactly
    /// to `self`.
    ///
    /// - Requires: `self` is an element of `String(utf8)!.indices`.
    @warn_unused_result
    public func samePositionIn(utf8: String.UTF8View) -> String.UTF8View.Index
    /// Return the position in `utf16` that corresponds exactly
    /// to `self`.
    ///
    /// - Requires: `self` is an element of `String(utf16)!.indices`.
    @warn_unused_result
    public func samePositionIn(utf16: String.UTF16View) -> String.UTF16View.Index
    /// Return the position in `characters` that corresponds exactly
    /// to `self`, or if no such position exists, `nil`.
    ///
    /// - Requires: `self` is an element of `characters.unicodeScalars.indices`.
    @warn_unused_result
    public func samePositionIn(characters: String) -> Index?
}

extension String.UTF16View.Index : BidirectionalIndexType {
    public typealias Distance = Int
    @warn_unused_result
    public func successor() -> String.UTF16View.Index
    @warn_unused_result
    public func predecessor() -> String.UTF16View.Index
}

extension String.UTF16View.Index : Comparable, Equatable {
}

extension String.UTF16View.Index {
    @warn_unused_result
    public func distanceTo(end: String.UTF16View.Index) -> Distance
    @warn_unused_result
    public func advancedBy(n: Distance) -> String.UTF16View.Index
    @warn_unused_result
    public func advancedBy(n: Distance, limit: String.UTF16View.Index) -> String.UTF16View.Index
}

extension String.UTF16View.Index {
    /// Construct the position in `utf16` that corresponds exactly to
    /// `utf8Index`. If no such position exists, the result is `nil`.
    ///
    /// - Requires: `utf8Index` is an element of
    ///   `String(utf16)!.utf8.indices`.
    public init?(_ utf8Index: UTF8Index, within utf16: String.UTF16View)
    /// Construct the position in `utf16` that corresponds exactly to
    /// `unicodeScalarIndex`.
    ///
    /// - Requires: `unicodeScalarIndex` is an element of
    ///   `String(utf16)!.unicodeScalars.indices`.
    public init(_ unicodeScalarIndex: UnicodeScalarIndex, within utf16: String.UTF16View)
    /// Construct the position in `utf16` that corresponds exactly to
    /// `characterIndex`.
    ///
    /// - Requires: `characterIndex` is an element of
    ///   `String(utf16)!.indices`.
    public init(_ characterIndex: Index, within utf16: String.UTF16View)
    /// Return the position in `utf8` that corresponds exactly
    /// to `self`, or if no such position exists, `nil`.
    ///
    /// - Requires: `self` is an element of
    ///   `String(utf8)!.utf16.indices`.
    @warn_unused_result
    public func samePositionIn(utf8: String.UTF8View) -> String.UTF8View.Index?
    /// Return the position in `unicodeScalars` that corresponds exactly
    /// to `self`, or if no such position exists, `nil`.
    ///
    /// - Requires: `self` is an element of
    ///   `String(unicodeScalars).utf16.indices`.
    @warn_unused_result
    public func samePositionIn(unicodeScalars: String.UnicodeScalarView) -> UnicodeScalarIndex?
    /// Return the position in `characters` that corresponds exactly
    /// to `self`, or if no such position exists, `nil`.
    ///
    /// - Requires: `self` is an element of `characters.utf16.indices`.
    @warn_unused_result
    public func samePositionIn(characters: String) -> Index?
}

extension String.UTF8View.Index {
    /// Construct the position in `utf8` that corresponds exactly to
    /// `utf16Index`. If no such position exists, the result is `nil`.
    ///
    /// - Requires: `utf8Index` is an element of
    ///   `String(utf16)!.utf8.indices`.
    public init?(_ utf16Index: UTF16Index, within utf8: String.UTF8View)
    /// Construct the position in `utf8` that corresponds exactly to
    /// `unicodeScalarIndex`.
    ///
    /// - Requires: `unicodeScalarIndex` is an element of
    ///   `String(utf8)!.unicodeScalars.indices`.
    public init(_ unicodeScalarIndex: UnicodeScalarIndex, within utf8: String.UTF8View)
    /// Construct the position in `utf8` that corresponds exactly to
    /// `characterIndex`.
    ///
    /// - Requires: `characterIndex` is an element of
    ///   `String(utf8)!.indices`.
    public init(_ characterIndex: Index, within utf8: String.UTF8View)
    /// Return the position in `utf16` that corresponds exactly
    /// to `self`, or if no such position exists, `nil`.
    ///
    /// - Requires: `self` is an element of `String(utf16)!.utf8.indices`.
    @warn_unused_result
    public func samePositionIn(utf16: String.UTF16View) -> String.UTF16View.Index?
    /// Return the position in `unicodeScalars` that corresponds exactly
    /// to `self`, or if no such position exists, `nil`.
    ///
    /// - Requires: `self` is an element of
    ///   `String(unicodeScalars).utf8.indices`.
    @warn_unused_result
    public func samePositionIn(unicodeScalars: String.UnicodeScalarView) -> UnicodeScalarIndex?
    /// Return the position in `characters` that corresponds exactly
    /// to `self`, or if no such position exists, `nil`.
    ///
    /// - Requires: `self` is an element of `characters.utf8.indices`.
    @warn_unused_result
    public func samePositionIn(characters: String) -> Index?
}

/// Conforming types can be initialized with string interpolations
/// containing `\(`...`)` clauses.
public protocol StringInterpolationConvertible {
    /// Create an instance by concatenating the elements of `strings`.
    public init(stringInterpolation strings: Self...)
    /// Create an instance containing `expr`'s `print` representation.
    public init<T>(stringInterpolationSegment expr: T)
}

/// Conforming types can be initialized with arbitrary string literals.
public protocol StringLiteralConvertible : ExtendedGraphemeClusterLiteralConvertible {
    typealias StringLiteralType
    /// Create an instance initialized to `value`.
    public init(stringLiteral value: Self.StringLiteralType)
}

/// The default type for an otherwise-unconstrained string literal.
public typealias StringLiteralType = String

/// A 64-bit unsigned integer value
/// type.
public struct UInt : UnsignedIntegerType, Comparable, Equatable {
    public var value: Builtin.Int64
    /// A type that can represent the number of steps between pairs of
    /// values.
    public typealias Distance = Int
    /// Create an instance initialized to zero.
    public init()
    public init(_ v: Builtin.Word)
    /// Create an instance initialized to `value`.
    public init(_ value: UInt)
    /// Creates an integer from its big-endian representation, changing the
    /// byte order if necessary.
    public init(bigEndian value: UInt)
    /// Creates an integer from its little-endian representation, changing the
    /// byte order if necessary.
    public init(littleEndian value: UInt)
    public init(_builtinIntegerLiteral value: Builtin.Int2048)
    /// Create an instance initialized to `value`.
    public init(integerLiteral value: UInt)
    /// Returns the big-endian representation of the integer, changing the
    /// byte order if necessary.
    public var bigEndian: UInt { get }
    /// Returns the little-endian representation of the integer, changing the
    /// byte order if necessary.
    public var littleEndian: UInt { get }
    /// Returns the current integer with the byte order swapped.
    public var byteSwapped: UInt { get }
    public static var max: UInt { get }
    public static var min: UInt { get }
}

extension UInt : Hashable {
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
}

extension UInt : CustomStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
}

extension UInt : RandomAccessIndexType {
    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    public func successor() -> UInt
    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    public func predecessor() -> UInt
    public func distanceTo(other: UInt) -> Distance
    public func advancedBy(n: Distance) -> UInt
}

extension UInt {
    /// Add `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func addWithOverflow(lhs: UInt, _ rhs: UInt) -> (UInt, overflow: Bool)
    /// Subtract `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func subtractWithOverflow(lhs: UInt, _ rhs: UInt) -> (UInt, overflow: Bool)
    /// Multiply `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func multiplyWithOverflow(lhs: UInt, _ rhs: UInt) -> (UInt, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning
    /// a result and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    public static func divideWithOverflow(lhs: UInt, _ rhs: UInt) -> (UInt, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning
    /// the remainder and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    public static func remainderWithOverflow(lhs: UInt, _ rhs: UInt) -> (UInt, overflow: Bool)
    /// Represent this number using Swift's widest native unsigned
    /// integer type.
    public func toUIntMax() -> UIntMax
    /// Explicitly convert to `IntMax`, trapping on overflow (except in -Ounchecked builds).
    public func toIntMax() -> IntMax
}

extension UInt {
    public init(_ v: UInt8)
    public init(_ v: Int8)
    public init(_ v: UInt16)
    public init(_ v: Int16)
    public init(_ v: UInt32)
    public init(_ v: Int32)
    public init(_ v: UInt64)
    /// Construct a `UInt` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: UInt64)
    public init(_ v: Int64)
    /// Construct a `UInt` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: Int64)
    public init(_ v: Int)
    /// Construct a `UInt` having the same memory representation as
    /// the `Int` `bitPattern`.  No range or overflow checking
    /// occurs, and the resulting `UInt` may not have the same numeric
    /// value as `bitPattern`--it is only guaranteed to use the same
    /// pattern of bits.
    public init(bitPattern: Int)
}

extension UInt : BitwiseOperationsType {
    /// The empty bitset of type UInt.
    public static var allZeros: UInt { get }
}

extension UInt {
    /// Construct an instance that approximates `other`.
    public init(_ other: Float)
    /// Construct an instance that approximates `other`.
    public init(_ other: Double)
    /// Construct an instance that approximates `other`.
    public init(_ other: Float80)
}

extension UInt {
    /// Construct from an ASCII representation in the given `radix`.
    ///
    /// If `text` does not match the regular expression
    /// "[+-][0-9a-zA-Z]+", or the value it denotes in the given `radix`
    /// is not representable, the result is `nil`.
    public init?(_ text: String, radix: Int = default)
}

extension UInt : _Reflectable {
}

extension UInt : CVarArgType {
}

/// A 16-bit unsigned integer value
/// type.
public struct UInt16 : UnsignedIntegerType, Comparable, Equatable {
    public var value: Builtin.Int16
    /// A type that can represent the number of steps between pairs of
    /// values.
    public typealias Distance = Int
    /// Create an instance initialized to zero.
    public init()
    /// Create an instance initialized to `value`.
    public init(_ value: UInt16)
    /// Creates an integer from its big-endian representation, changing the
    /// byte order if necessary.
    public init(bigEndian value: UInt16)
    /// Creates an integer from its little-endian representation, changing the
    /// byte order if necessary.
    public init(littleEndian value: UInt16)
    public init(_builtinIntegerLiteral value: Builtin.Int2048)
    /// Create an instance initialized to `value`.
    public init(integerLiteral value: UInt16)
    /// Returns the big-endian representation of the integer, changing the
    /// byte order if necessary.
    public var bigEndian: UInt16 { get }
    /// Returns the little-endian representation of the integer, changing the
    /// byte order if necessary.
    public var littleEndian: UInt16 { get }
    /// Returns the current integer with the byte order swapped.
    public var byteSwapped: UInt16 { get }
    public static var max: UInt16 { get }
    public static var min: UInt16 { get }
}

extension UInt16 : Hashable {
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
}

extension UInt16 : CustomStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
}

extension UInt16 : RandomAccessIndexType {
    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    public func successor() -> UInt16
    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    public func predecessor() -> UInt16
    public func distanceTo(other: UInt16) -> Distance
    public func advancedBy(n: Distance) -> UInt16
}

extension UInt16 {
    /// Add `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func addWithOverflow(lhs: UInt16, _ rhs: UInt16) -> (UInt16, overflow: Bool)
    /// Subtract `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func subtractWithOverflow(lhs: UInt16, _ rhs: UInt16) -> (UInt16, overflow: Bool)
    /// Multiply `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func multiplyWithOverflow(lhs: UInt16, _ rhs: UInt16) -> (UInt16, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning
    /// a result and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    public static func divideWithOverflow(lhs: UInt16, _ rhs: UInt16) -> (UInt16, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning
    /// the remainder and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    public static func remainderWithOverflow(lhs: UInt16, _ rhs: UInt16) -> (UInt16, overflow: Bool)
    /// Represent this number using Swift's widest native unsigned
    /// integer type.
    public func toUIntMax() -> UIntMax
    /// Explicitly convert to `IntMax`.
    public func toIntMax() -> IntMax
}

extension UInt16 {
    public init(_ v: UInt8)
    public init(_ v: Int8)
    public init(_ v: Int16)
    public init(_ v: UInt32)
    /// Construct a `UInt16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: UInt32)
    public init(_ v: Int32)
    /// Construct a `UInt16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: Int32)
    public init(_ v: UInt64)
    /// Construct a `UInt16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: UInt64)
    public init(_ v: Int64)
    /// Construct a `UInt16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: Int64)
    public init(_ v: UInt)
    /// Construct a `UInt16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: UInt)
    public init(_ v: Int)
    /// Construct a `UInt16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: Int)
    /// Construct a `UInt16` having the same memory representation as
    /// the `Int16` `bitPattern`.  No range or overflow checking
    /// occurs, and the resulting `UInt16` may not have the same numeric
    /// value as `bitPattern`--it is only guaranteed to use the same
    /// pattern of bits.
    public init(bitPattern: Int16)
}

extension UInt16 : BitwiseOperationsType {
    /// The empty bitset of type UInt16.
    public static var allZeros: UInt16 { get }
}

extension UInt16 {
    /// Construct an instance that approximates `other`.
    public init(_ other: Float)
    /// Construct an instance that approximates `other`.
    public init(_ other: Double)
    /// Construct an instance that approximates `other`.
    public init(_ other: Float80)
}

extension UInt16 {
    /// Construct from an ASCII representation in the given `radix`.
    ///
    /// If `text` does not match the regular expression
    /// "[+-][0-9a-zA-Z]+", or the value it denotes in the given `radix`
    /// is not representable, the result is `nil`.
    public init?(_ text: String, radix: Int = default)
}

extension UInt16 : _Reflectable {
}

extension UInt16 : _StringElementType {
}

extension UInt16 : CVarArgType {
}

/// A 32-bit unsigned integer value
/// type.
public struct UInt32 : UnsignedIntegerType, Comparable, Equatable {
    public var value: Builtin.Int32
    /// A type that can represent the number of steps between pairs of
    /// values.
    public typealias Distance = Int
    /// Create an instance initialized to zero.
    public init()
    /// Create an instance initialized to `value`.
    public init(_ value: UInt32)
    /// Creates an integer from its big-endian representation, changing the
    /// byte order if necessary.
    public init(bigEndian value: UInt32)
    /// Creates an integer from its little-endian representation, changing the
    /// byte order if necessary.
    public init(littleEndian value: UInt32)
    public init(_builtinIntegerLiteral value: Builtin.Int2048)
    /// Create an instance initialized to `value`.
    public init(integerLiteral value: UInt32)
    /// Returns the big-endian representation of the integer, changing the
    /// byte order if necessary.
    public var bigEndian: UInt32 { get }
    /// Returns the little-endian representation of the integer, changing the
    /// byte order if necessary.
    public var littleEndian: UInt32 { get }
    /// Returns the current integer with the byte order swapped.
    public var byteSwapped: UInt32 { get }
    public static var max: UInt32 { get }
    public static var min: UInt32 { get }
}

extension UInt32 : Hashable {
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
}

extension UInt32 : CustomStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
}

extension UInt32 : RandomAccessIndexType {
    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    public func successor() -> UInt32
    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    public func predecessor() -> UInt32
    public func distanceTo(other: UInt32) -> Distance
    public func advancedBy(n: Distance) -> UInt32
}

extension UInt32 {
    /// Add `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func addWithOverflow(lhs: UInt32, _ rhs: UInt32) -> (UInt32, overflow: Bool)
    /// Subtract `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func subtractWithOverflow(lhs: UInt32, _ rhs: UInt32) -> (UInt32, overflow: Bool)
    /// Multiply `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func multiplyWithOverflow(lhs: UInt32, _ rhs: UInt32) -> (UInt32, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning
    /// a result and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    public static func divideWithOverflow(lhs: UInt32, _ rhs: UInt32) -> (UInt32, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning
    /// the remainder and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    public static func remainderWithOverflow(lhs: UInt32, _ rhs: UInt32) -> (UInt32, overflow: Bool)
    /// Represent this number using Swift's widest native unsigned
    /// integer type.
    public func toUIntMax() -> UIntMax
    /// Explicitly convert to `IntMax`.
    public func toIntMax() -> IntMax
}

extension UInt32 {
    public init(_ v: UInt8)
    public init(_ v: Int8)
    public init(_ v: UInt16)
    public init(_ v: Int16)
    public init(_ v: Int32)
    public init(_ v: UInt64)
    /// Construct a `UInt32` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: UInt64)
    public init(_ v: Int64)
    /// Construct a `UInt32` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: Int64)
    public init(_ v: UInt)
    /// Construct a `UInt32` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: UInt)
    public init(_ v: Int)
    /// Construct a `UInt32` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: Int)
    /// Construct a `UInt32` having the same memory representation as
    /// the `Int32` `bitPattern`.  No range or overflow checking
    /// occurs, and the resulting `UInt32` may not have the same numeric
    /// value as `bitPattern`--it is only guaranteed to use the same
    /// pattern of bits.
    public init(bitPattern: Int32)
}

extension UInt32 : BitwiseOperationsType {
    /// The empty bitset of type UInt32.
    public static var allZeros: UInt32 { get }
}

extension UInt32 {
    /// Construct an instance that approximates `other`.
    public init(_ other: Float)
    /// Construct an instance that approximates `other`.
    public init(_ other: Double)
    /// Construct an instance that approximates `other`.
    public init(_ other: Float80)
}

extension UInt32 {
    /// Construct from an ASCII representation in the given `radix`.
    ///
    /// If `text` does not match the regular expression
    /// "[+-][0-9a-zA-Z]+", or the value it denotes in the given `radix`
    /// is not representable, the result is `nil`.
    public init?(_ text: String, radix: Int = default)
}

extension UInt32 : _Reflectable {
}

extension UInt32 {
    /// Construct with value `v.value`.
    ///
    /// - Requires: `v.value` can be represented as UInt32.
    public init(_ v: UnicodeScalar)
}

extension UInt32 : CVarArgType {
}

/// A 64-bit unsigned integer value
/// type.
public struct UInt64 : UnsignedIntegerType, Comparable, Equatable {
    public var value: Builtin.Int64
    /// A type that can represent the number of steps between pairs of
    /// values.
    public typealias Distance = Int
    /// Create an instance initialized to zero.
    public init()
    /// Create an instance initialized to `value`.
    public init(_ value: UInt64)
    /// Creates an integer from its big-endian representation, changing the
    /// byte order if necessary.
    public init(bigEndian value: UInt64)
    /// Creates an integer from its little-endian representation, changing the
    /// byte order if necessary.
    public init(littleEndian value: UInt64)
    public init(_builtinIntegerLiteral value: Builtin.Int2048)
    /// Create an instance initialized to `value`.
    public init(integerLiteral value: UInt64)
    /// Returns the big-endian representation of the integer, changing the
    /// byte order if necessary.
    public var bigEndian: UInt64 { get }
    /// Returns the little-endian representation of the integer, changing the
    /// byte order if necessary.
    public var littleEndian: UInt64 { get }
    /// Returns the current integer with the byte order swapped.
    public var byteSwapped: UInt64 { get }
    public static var max: UInt64 { get }
    public static var min: UInt64 { get }
}

extension UInt64 : Hashable {
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
}

extension UInt64 : CustomStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
}

extension UInt64 : RandomAccessIndexType {
    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    public func successor() -> UInt64
    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    public func predecessor() -> UInt64
    public func distanceTo(other: UInt64) -> Distance
    public func advancedBy(n: Distance) -> UInt64
}

extension UInt64 {
    /// Add `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func addWithOverflow(lhs: UInt64, _ rhs: UInt64) -> (UInt64, overflow: Bool)
    /// Subtract `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func subtractWithOverflow(lhs: UInt64, _ rhs: UInt64) -> (UInt64, overflow: Bool)
    /// Multiply `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func multiplyWithOverflow(lhs: UInt64, _ rhs: UInt64) -> (UInt64, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning
    /// a result and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    public static func divideWithOverflow(lhs: UInt64, _ rhs: UInt64) -> (UInt64, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning
    /// the remainder and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    public static func remainderWithOverflow(lhs: UInt64, _ rhs: UInt64) -> (UInt64, overflow: Bool)
    /// Represent this number using Swift's widest native unsigned
    /// integer type.
    public func toUIntMax() -> UIntMax
    /// Explicitly convert to `IntMax`, trapping on overflow (except in -Ounchecked builds).
    public func toIntMax() -> IntMax
}

extension UInt64 {
    public init(_ v: UInt8)
    public init(_ v: Int8)
    public init(_ v: UInt16)
    public init(_ v: Int16)
    public init(_ v: UInt32)
    public init(_ v: Int32)
    public init(_ v: Int64)
    public init(_ v: UInt)
    public init(_ v: Int)
    /// Construct a `UInt64` having the same memory representation as
    /// the `Int64` `bitPattern`.  No range or overflow checking
    /// occurs, and the resulting `UInt64` may not have the same numeric
    /// value as `bitPattern`--it is only guaranteed to use the same
    /// pattern of bits.
    public init(bitPattern: Int64)
}

extension UInt64 : BitwiseOperationsType {
    /// The empty bitset of type UInt64.
    public static var allZeros: UInt64 { get }
}

extension UInt64 {
    /// Construct an instance that approximates `other`.
    public init(_ other: Float)
    /// Construct an instance that approximates `other`.
    public init(_ other: Double)
    /// Construct an instance that approximates `other`.
    public init(_ other: Float80)
}

extension UInt64 {
    /// Construct from an ASCII representation in the given `radix`.
    ///
    /// If `text` does not match the regular expression
    /// "[+-][0-9a-zA-Z]+", or the value it denotes in the given `radix`
    /// is not representable, the result is `nil`.
    public init?(_ text: String, radix: Int = default)
}

extension UInt64 : _Reflectable {
}

extension UInt64 {
    /// Construct with value `v.value`.
    ///
    /// - Requires: `v.value` can be represented as UInt64.
    public init(_ v: UnicodeScalar)
}

extension UInt64 : CVarArgType, _CVarArgAlignedType {
}

/// A 8-bit unsigned integer value
/// type.
public struct UInt8 : UnsignedIntegerType, Comparable, Equatable {
    public var value: Builtin.Int8
    /// A type that can represent the number of steps between pairs of
    /// values.
    public typealias Distance = Int
    /// Create an instance initialized to zero.
    public init()
    /// Create an instance initialized to `value`.
    public init(_ value: UInt8)
    public init(_builtinIntegerLiteral value: Builtin.Int2048)
    /// Create an instance initialized to `value`.
    public init(integerLiteral value: UInt8)
    public static var max: UInt8 { get }
    public static var min: UInt8 { get }
}

extension UInt8 : Hashable {
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
}

extension UInt8 : CustomStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
}

extension UInt8 : RandomAccessIndexType {
    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    public func successor() -> UInt8
    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    public func predecessor() -> UInt8
    public func distanceTo(other: UInt8) -> Distance
    public func advancedBy(n: Distance) -> UInt8
}

extension UInt8 {
    /// Add `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func addWithOverflow(lhs: UInt8, _ rhs: UInt8) -> (UInt8, overflow: Bool)
    /// Subtract `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func subtractWithOverflow(lhs: UInt8, _ rhs: UInt8) -> (UInt8, overflow: Bool)
    /// Multiply `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    public static func multiplyWithOverflow(lhs: UInt8, _ rhs: UInt8) -> (UInt8, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning
    /// a result and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    public static func divideWithOverflow(lhs: UInt8, _ rhs: UInt8) -> (UInt8, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning
    /// the remainder and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    public static func remainderWithOverflow(lhs: UInt8, _ rhs: UInt8) -> (UInt8, overflow: Bool)
    /// Represent this number using Swift's widest native unsigned
    /// integer type.
    public func toUIntMax() -> UIntMax
    /// Explicitly convert to `IntMax`.
    public func toIntMax() -> IntMax
}

extension UInt8 {
    public init(_ v: Int8)
    public init(_ v: UInt16)
    /// Construct a `UInt8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: UInt16)
    public init(_ v: Int16)
    /// Construct a `UInt8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: Int16)
    public init(_ v: UInt32)
    /// Construct a `UInt8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: UInt32)
    public init(_ v: Int32)
    /// Construct a `UInt8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: Int32)
    public init(_ v: UInt64)
    /// Construct a `UInt8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: UInt64)
    public init(_ v: Int64)
    /// Construct a `UInt8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: Int64)
    public init(_ v: UInt)
    /// Construct a `UInt8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: UInt)
    public init(_ v: Int)
    /// Construct a `UInt8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    public init(truncatingBitPattern: Int)
    /// Construct a `UInt8` having the same memory representation as
    /// the `Int8` `bitPattern`.  No range or overflow checking
    /// occurs, and the resulting `UInt8` may not have the same numeric
    /// value as `bitPattern`--it is only guaranteed to use the same
    /// pattern of bits.
    public init(bitPattern: Int8)
}

extension UInt8 : BitwiseOperationsType {
    /// The empty bitset of type UInt8.
    public static var allZeros: UInt8 { get }
}

extension UInt8 {
    /// Construct an instance that approximates `other`.
    public init(_ other: Float)
    /// Construct an instance that approximates `other`.
    public init(_ other: Double)
    /// Construct an instance that approximates `other`.
    public init(_ other: Float80)
}

extension UInt8 {
    /// Construct from an ASCII representation in the given `radix`.
    ///
    /// If `text` does not match the regular expression
    /// "[+-][0-9a-zA-Z]+", or the value it denotes in the given `radix`
    /// is not representable, the result is `nil`.
    public init?(_ text: String, radix: Int = default)
}

extension UInt8 : _Reflectable {
}

extension UInt8 : _StringElementType {
}

extension UInt8 {
    /// Construct with value `v.value`.
    ///
    /// - Requires: `v.value` can be represented as ASCII (0..<128).
    public init(ascii v: UnicodeScalar)
}

extension UInt8 : CVarArgType {
}

/// The largest native unsigned integer type.
public typealias UIntMax = UInt64

/// A codec for [UTF-16](http://www.unicode.org/glossary/#UTF_16).
public struct UTF16 : UnicodeCodecType {
    /// A type that can hold [code unit](http://www.unicode.org/glossary/#code_unit) values for this
    /// encoding.
    public typealias CodeUnit = UInt16
    public init()
    /// Start or continue decoding a UTF sequence.
    ///
    /// In order to decode a code unit sequence completely, this function should
    /// be called repeatedly until it returns `UnicodeDecodingResult.EmptyInput`.
    /// Checking that the generator was exhausted is not sufficient.  The decoder
    /// can have an internal buffer that is pre-filled with data from the input
    /// generator.
    ///
    /// Because of buffering, it is impossible to find the corresponding position
    /// in the generator for a given returned `UnicodeScalar` or an error.
    ///
    /// - parameter next: A *generator* of code units to be decoded.
    public mutating func decode<G : GeneratorType where G.Element == CodeUnit>(inout input: G) -> UnicodeDecodingResult
    /// Encode a `UnicodeScalar` as a series of `CodeUnit`s by
    /// calling `output` on each `CodeUnit`.
    public static func encode(input: UnicodeScalar, output put: (CodeUnit) -> ())
}

extension UTF16 {
    /// Return the number of code units required to encode `x`.
    @warn_unused_result
    public static func width(x: UnicodeScalar) -> Int
    /// Return the high surrogate code unit of a [surrogate pair](http://www.unicode.org/glossary/#surrogate_pair) representing
    /// `x`.
    ///
    /// - Requires: `width(x) == 2`.
    @warn_unused_result
    public static func leadSurrogate(x: UnicodeScalar) -> CodeUnit
    /// Return the low surrogate code unit of a [surrogate pair](http://www.unicode.org/glossary/#surrogate_pair) representing
    /// `x`.
    ///
    /// - Requires: `width(x) == 2`.
    @warn_unused_result
    public static func trailSurrogate(x: UnicodeScalar) -> CodeUnit
    @warn_unused_result
    public static func isLeadSurrogate(x: CodeUnit) -> Bool
    @warn_unused_result
    public static func isTrailSurrogate(x: CodeUnit) -> Bool
    /// Returns the number of UTF-16 code units required for the given code unit
    /// sequence when transcoded to UTF-16, and a bit describing if the sequence
    /// was found to contain only ASCII characters.
    ///
    /// If `repairIllFormedSequences` is `true`, the function always succeeds.
    /// If it is `false`, `nil` is returned if an ill-formed code unit sequence is
    /// found in `input`.
    @warn_unused_result
    public static func measure<Encoding : UnicodeCodecType, Input : GeneratorType where Encoding.CodeUnit == Input.Element>(_: Encoding.Type, input: Input, repairIllFormedSequences: Bool) -> (Int, Bool)?
}

/// A codec for [UTF-32](http://www.unicode.org/glossary/#UTF_32).
public struct UTF32 : UnicodeCodecType {
    /// A type that can hold [code unit](http://www.unicode.org/glossary/#code_unit) values for this
    /// encoding.
    public typealias CodeUnit = UInt32
    public init()
    /// Start or continue decoding a UTF sequence.
    ///
    /// In order to decode a code unit sequence completely, this function should
    /// be called repeatedly until it returns `UnicodeDecodingResult.EmptyInput`.
    /// Checking that the generator was exhausted is not sufficient.  The decoder
    /// can have an internal buffer that is pre-filled with data from the input
    /// generator.
    ///
    /// Because of buffering, it is impossible to find the corresponding position
    /// in the generator for a given returned `UnicodeScalar` or an error.
    ///
    /// - parameter next: A *generator* of code units to be decoded.
    public mutating func decode<G : GeneratorType where G.Element == CodeUnit>(inout input: G) -> UnicodeDecodingResult
    /// Encode a `UnicodeScalar` as a series of `CodeUnit`s by
    /// calling `output` on each `CodeUnit`.
    public static func encode(input: UnicodeScalar, output put: (CodeUnit) -> ())
}

/// A codec for [UTF-8](http://www.unicode.org/glossary/#UTF_8).
public struct UTF8 : UnicodeCodecType {
    /// A type that can hold [code unit](http://www.unicode.org/glossary/#code_unit) values for this
    /// encoding.
    public typealias CodeUnit = UInt8
    public init()
    /// Start or continue decoding a UTF sequence.
    ///
    /// In order to decode a code unit sequence completely, this function should
    /// be called repeatedly until it returns `UnicodeDecodingResult.EmptyInput`.
    /// Checking that the generator was exhausted is not sufficient.  The decoder
    /// can have an internal buffer that is pre-filled with data from the input
    /// generator.
    ///
    /// Because of buffering, it is impossible to find the corresponding position
    /// in the generator for a given returned `UnicodeScalar` or an error.
    ///
    /// - parameter next: A *generator* of code units to be decoded.
    public mutating func decode<G : GeneratorType where G.Element == CodeUnit>(inout next: G) -> UnicodeDecodingResult
    /// Encode a `UnicodeScalar` as a series of `CodeUnit`s by
    /// calling `output` on each `CodeUnit`.
    public static func encode(input: UnicodeScalar, output put: (CodeUnit) -> ())
    /// Return `true` if `byte` is a continuation byte of the form
    /// `0b10xxxxxx`.
    @warn_unused_result
    public static func isContinuation(byte: CodeUnit) -> Bool
}

/// A Unicode [encoding scheme](http://www.unicode.org/glossary/#character_encoding_scheme).
///
/// Consists of an underlying [code unit](http://www.unicode.org/glossary/#code_unit) and functions to
/// translate between sequences of these code units and [unicode scalar values](http://www.unicode.org/glossary/#unicode_scalar_value).
public protocol UnicodeCodecType {
    /// A type that can hold [code unit](http://www.unicode.org/glossary/#code_unit) values for this
    /// encoding.
    typealias CodeUnit
    public init()
    /// Start or continue decoding a UTF sequence.
    ///
    /// In order to decode a code unit sequence completely, this function should
    /// be called repeatedly until it returns `UnicodeDecodingResult.EmptyInput`.
    /// Checking that the generator was exhausted is not sufficient.  The decoder
    /// can have an internal buffer that is pre-filled with data from the input
    /// generator.
    ///
    /// Because of buffering, it is impossible to find the corresponding position
    /// in the generator for a given returned `UnicodeScalar` or an error.
    ///
    /// - parameter next: A *generator* of code units to be decoded.
    public mutating func decode<G : GeneratorType where G.Element == CodeUnit>(inout next: G) -> UnicodeDecodingResult
    /// Encode a `UnicodeScalar` as a series of `CodeUnit`s by
    /// calling `output` on each `CodeUnit`.
    public static func encode(input: UnicodeScalar, output: (Self.CodeUnit) -> ())
}

/// The result of one Unicode decoding step.
///
/// A unicode scalar value, an indication that no more unicode scalars
/// are available, or an indication of a decoding error.
public enum UnicodeDecodingResult {
    case Result(UnicodeScalar)
    case EmptyInput
    case Error
    /// Return true if `self` indicates no more unicode scalars are
    /// available.
    @warn_unused_result
    public func isEmptyInput() -> Bool
}

/// A [Unicode scalar value](http://www.unicode.org/glossary/#unicode_scalar_value).
public struct UnicodeScalar : UnicodeScalarLiteralConvertible {
    /// A numeric representation of `self`.
    public var value: UInt32 { get }
    public init(_builtinUnicodeScalarLiteral value: Builtin.Int32)
    /// Create an instance initialized to `value`.
    public init(unicodeScalarLiteral value: UnicodeScalar)
    /// Creates an instance of the NUL scalar value.
    public init()
    /// Create an instance with numeric value `v`.
    ///
    /// - Requires: `v` is a valid Unicode scalar value.
    public init(_ v: UInt32)
    /// Create an instance with numeric value `v`.
    ///
    /// - Requires: `v` is a valid Unicode scalar value.
    public init(_ v: UInt16)
    /// Create an instance with numeric value `v`.
    public init(_ v: UInt8)
    /// Create a duplicate of `v`.
    public init(_ v: UnicodeScalar)
    /// Return a String representation of `self` .
    ///
    /// - parameter forceASCII: If `true`, forces most values into a numeric
    ///   representation.
    @warn_unused_result
    public func escape(asASCII forceASCII: Bool) -> String
    /// Returns true if this is an ASCII character (code point 0 to 127
    /// inclusive).
    @warn_unused_result
    public func isASCII() -> Bool
}

extension UnicodeScalar : _Reflectable {
}

extension UnicodeScalar : Streamable {
    /// Write a textual representation of `self` into `target`.
    public func writeTo<Target : OutputStreamType>(inout target: Target)
}

extension UnicodeScalar : CustomStringConvertible, CustomDebugStringConvertible {
    /// A textual representation of `self`.
    public var description: String { get }
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
}

extension UnicodeScalar : Hashable {
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
}

extension UnicodeScalar {
    /// Construct with value `v`.
    ///
    /// - Requires: `v` is a valid unicode scalar value.
    public init(_ v: Int)
}

extension UnicodeScalar : Comparable, Equatable {
}



extension UnicodeScalar.UTF16View : CollectionType {
}

/// Conforming types can be initialized with string literals
/// containing a single [Unicode scalar value](http://www.unicode.org/glossary/#unicode_scalar_value).
public protocol UnicodeScalarLiteralConvertible {
    typealias UnicodeScalarLiteralType
    /// Create an instance initialized to `value`.
    public init(unicodeScalarLiteral value: Self.UnicodeScalarLiteralType)
}

/// The default type for an otherwise-unconstrained unicode scalar literal.
public typealias UnicodeScalarType = String

/// A type for propagating an unmanaged object reference.
///
/// When you use this type, you become partially responsible for
/// keeping the object alive.
public struct Unmanaged<Instance : AnyObject> {
    /// Unsafely turn an opaque C pointer into an unmanaged
    /// class reference.
    ///
    /// This operation does not change reference counts.
    ///
    ///     let str: CFString = Unmanaged.fromOpaque(ptr).takeUnretainedValue()
    @warn_unused_result
    public static func fromOpaque(value: COpaquePointer) -> Unmanaged<Instance>
    /// Unsafely turn an unmanaged class reference into an opaque
    /// C pointer.
    ///
    /// This operation does not change reference counts.
    ///
    ///     let str: CFString = Unmanaged.fromOpaque(ptr).takeUnretainedValue()
    @warn_unused_result
    public func toOpaque() -> COpaquePointer
    /// Create an unmanaged reference with an unbalanced retain.
    /// The object will leak if nothing eventually balances the retain.
    ///
    /// This is useful when passing an object to an API which Swift
    /// does not know the ownership rules for, but you know that the
    /// API expects you to pass the object at +1.
    @warn_unused_result
    public static func passRetained(value: Instance) -> Unmanaged<Instance>
    /// Create an unmanaged reference without performing an unbalanced
    /// retain.
    ///
    /// This is useful when passing a reference to an API which Swift
    /// does not know the ownership rules for, but you know that the
    /// API expects you to pass the object at +0.
    ///
    ///     CFArraySetValueAtIndex(.passUnretained(array), i,
    ///                            .passUnretained(object))
    @warn_unused_result
    public static func passUnretained(value: Instance) -> Unmanaged<Instance>
    /// Get the value of this unmanaged reference as a managed
    /// reference without consuming an unbalanced retain of it.
    ///
    /// This is useful when a function returns an unmanaged reference
    /// and you know that you're not responsible for releasing the result.
    @warn_unused_result
    public func takeUnretainedValue() -> Instance
    /// Get the value of this unmanaged reference as a managed
    /// reference and consume an unbalanced retain of it.
    ///
    /// This is useful when a function returns an unmanaged reference
    /// and you know that you're responsible for releasing the result.
    @warn_unused_result
    public func takeRetainedValue() -> Instance
    /// Perform an unbalanced retain of the object.
    public func retain() -> Unmanaged<Instance>
    /// Perform an unbalanced release of the object.
    public func release()
    /// Perform an unbalanced autorelease of the object.
    public func autorelease() -> Unmanaged<Instance>
}

/// A non-owning pointer to buffer of  `Element`s stored
/// contiguously in memory, presenting a `Collection` interface to the
/// underlying elements.
///
/// The pointer should be aligned to `alignof(Element.self)`.
public struct UnsafeBufferPointer<Element> : CollectionType {
    /// Always zero, which is the index of the first element in a
    /// non-empty buffer.
    public var startIndex: Int { get }
    /// The "past the end" position; always identical to `count`.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    public var endIndex: Int { get }
    public subscript (i: Int) -> Element { get }
    /// Construct an UnsafePointer over the `count` contiguous
    /// `Element` instances beginning at `start`.
    public init(start: UnsafePointer<Element>, count: Int)
    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    public func generate() -> UnsafeBufferPointerGenerator<Element>
    /// A pointer to the first element of the buffer.
    public var baseAddress: UnsafePointer<Element> { get }
    /// The number of elements in the buffer.
    public var count: Int { get }
}

extension UnsafeBufferPointer : CustomDebugStringConvertible {
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
}

/// A generator for the elements in the buffer referenced by
/// `UnsafeBufferPointer` or `UnsafeMutableBufferPointer`.
public struct UnsafeBufferPointerGenerator<Element> : GeneratorType, SequenceType {
    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    public mutating func next() -> Element?
}

/// A non-owning pointer to buffer of mutable `Element`s stored
/// contiguously in memory, presenting a `Collection` interface to the
/// underlying elements.
///
/// The pointer should be aligned to `alignof(Element.self)`.
public struct UnsafeMutableBufferPointer<Element> : MutableCollectionType {
    /// Always zero, which is the index of the first element in a
    /// non-empty buffer.
    public var startIndex: Int { get }
    /// The "past the end" position; always identical to `count`.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    public var endIndex: Int { get }
    public subscript (i: Int) -> Element { get nonmutating set }
    /// Construct an UnsafeMutablePointer over the `count` contiguous
    /// `Element` instances beginning at `start`.
    public init(start: UnsafeMutablePointer<Element>, count: Int)
    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    public func generate() -> UnsafeBufferPointerGenerator<Element>
    /// A pointer to the first element of the buffer.
    public var baseAddress: UnsafeMutablePointer<Element> { get }
    /// The number of elements in the buffer.
    public var count: Int { get }
}

extension UnsafeMutableBufferPointer : CustomDebugStringConvertible {
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
}

/// A pointer to an object of type `Memory`.  This type provides no automated
/// memory management, and therefore the user must take care to allocate
/// and free memory appropriately.
///
/// The pointer should be aligned to `alignof(Memory.self)`.
///
/// The pointer can be in one of the following states:
///
/// - memory is not allocated (for example, pointer is null, or memory has
///   been deallocated previously);
///
/// - memory is allocated, but value has not been initialized;
///
/// - memory is allocated and value is initialized.
public struct UnsafeMutablePointer<Memory> : RandomAccessIndexType, Hashable, NilLiteralConvertible, _PointerType {
    public typealias Distance = Int
    /// Construct a null pointer.
    public init()
    /// Convert from an opaque C pointer to a typed pointer.
    ///
    /// This is a fundamentally unsafe conversion.
    public init(_ other: COpaquePointer)
    /// Construct an `UnsafeMutablePointer` from a given address in memory.
    ///
    /// This is a fundamentally unsafe conversion.
    public init(bitPattern: Int)
    /// Construct an `UnsafeMutablePointer` from a given address in memory.
    ///
    /// This is a fundamentally unsafe conversion.
    public init(bitPattern: UInt)
    /// Convert from an `UnsafeMutablePointer` of a different type.
    ///
    /// This is a fundamentally unsafe conversion.
    public init<U>(_ from: UnsafeMutablePointer<U>)
    /// Convert from a `UnsafePointer` of a different type.
    ///
    /// This is a fundamentally unsafe conversion.
    public init<U>(_ from: UnsafePointer<U>)
    /// Create an instance initialized with `nil`.
    public init(nilLiteral: ())
    /// Allocate memory for `num` objects of type `Memory`.
    ///
    /// - Postcondition: The memory is allocated, but not initialized.
    @warn_unused_result
    public static func alloc(num: Int) -> UnsafeMutablePointer<Memory>
    /// Deallocate `num` objects.
    ///
    /// - parameter num: Number of objects to deallocate.  Should match exactly
    ///   the value that was passed to `alloc()` (partial deallocations are not
    ///   possible).
    ///
    /// - Precondition: The memory is not initialized.
    ///
    /// - Postcondition: The memory has been deallocated.
    public func dealloc(num: Int)
    /// Access the underlying raw memory, getting and setting values.
    public var memory: Memory { get nonmutating set }
    /// Initialize the value the pointer points to, to construct
    /// an object where there was no object previously stored.
    ///
    /// - Precondition: The memory is not initialized.
    ///
    /// - Postcondition: The memory is initalized; the value should eventually
    ///   be destroyed or moved from to avoid leaks.
    public func initialize(newvalue: Memory)
    /// Retrieve the value the pointer points to, moving it away
    /// from the location referenced in memory.
    ///
    /// Equivalent to reading `memory` property and calling `destroy()`,
    /// but more efficient.
    ///
    /// - Precondition: The memory is initialized.
    ///
    /// - Postcondition: The value has been destroyed and the memory must
    ///   be initialized before being used again.
    @warn_unused_result
    public func move() -> Memory
    /// Assign from `count` values beginning at source into initialized
    /// memory, proceeding from the first element to the last.
    public func assignFrom(source: UnsafeMutablePointer<Memory>, count: Int)
    /// Assign from `count` values beginning at `source` into
    /// initialized memory, proceeding from the last value to the first.
    /// Use this for assigning ranges into later memory that may overlap
    /// with the source range.
    ///
    /// - Requires: Either `source` precedes `self` or follows `self + count`.
    public func assignBackwardFrom(source: UnsafeMutablePointer<Memory>, count: Int)
    /// Move `count` values beginning at source into raw memory,
    /// transforming the source values into raw memory.
    public func moveInitializeFrom(source: UnsafeMutablePointer<Memory>, count: Int)
    /// Move `count` values beginning at `source` into uninitialized memory,
    /// transforming the source values into raw memory, proceeding from
    /// the last value to the first.  Use this for copying ranges into
    /// later memory that may overlap with the source range.
    ///
    /// - Requires: Either `source` precedes `self` or follows `self + count`.
    public func moveInitializeBackwardFrom(source: UnsafeMutablePointer<Memory>, count: Int)
    /// Copy `count` values beginning at source into raw memory.
    ///
    /// - Precondition: The memory is not initialized.
    ///
    /// - Requires: `self` and `source` may not overlap.
    public func initializeFrom(source: UnsafeMutablePointer<Memory>, count: Int)
    /// Copy the elements of `C` into raw memory.
    ///
    /// - Precondition: The memory is not initialized.
    public func initializeFrom<C : CollectionType where C.Generator.Element == Memory>(source: C)
    /// Assign from `count` values beginning at `source` into initialized
    /// memory, transforming the source values into raw memory.
    ///
    /// - Requires: The `self` and `source` ranges may not overlap.
    public func moveAssignFrom(source: UnsafeMutablePointer<Memory>, count: Int)
    /// Destroy the object the pointer points to.
    ///
    /// - Precondition: The memory is initialized.
    ///
    /// - Postcondition: The value has been destroyed and the memory must
    ///   be initialized before being used again.
    public func destroy()
    /// Destroy the `count` objects the pointer points to.
    /// - Precondition: The memory is initialized.
    ///
    /// - Postcondition: The value has been destroyed and the memory must
    ///   be initialized before being used again.
    public func destroy(count: Int)
    public subscript (i: Int) -> Memory { get nonmutating set }
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    public func successor() -> UnsafeMutablePointer<Memory>
    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    public func predecessor() -> UnsafeMutablePointer<Memory>
    public func distanceTo(x: UnsafeMutablePointer<Memory>) -> Int
    public func advancedBy(n: Int) -> UnsafeMutablePointer<Memory>
}

extension UnsafeMutablePointer : CustomDebugStringConvertible {
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
}

extension UnsafeMutablePointer : _Reflectable {
}

extension UnsafeMutablePointer : CVarArgType {
}

/// A pointer to an object of type `Memory`.  This type provides no automated
/// memory management, and therefore the user must take care to allocate
/// and free memory appropriately.
///
/// The pointer should be aligned to `alignof(Memory.self)`.
///
/// The pointer can be in one of the following states:
///
/// - memory is not allocated (for example, pointer is null, or memory has
///   been deallocated previously);
///
/// - memory is allocated, but value has not been initialized;
///
/// - memory is allocated and value is initialized.
public struct UnsafePointer<Memory> : RandomAccessIndexType, Hashable, NilLiteralConvertible, _PointerType {
    public typealias Distance = Int
    /// Construct a null pointer.
    public init()
    /// Convert from an opaque C pointer to a typed pointer.
    ///
    /// This is a fundamentally unsafe conversion.
    public init(_ other: COpaquePointer)
    /// Construct an `UnsafePointer` from a given address in memory.
    ///
    /// This is a fundamentally unsafe conversion.
    public init(bitPattern: Int)
    /// Construct an `UnsafePointer` from a given address in memory.
    ///
    /// This is a fundamentally unsafe conversion.
    public init(bitPattern: UInt)
    /// Convert from an `UnsafeMutablePointer` of a different type.
    ///
    /// This is a fundamentally unsafe conversion.
    public init<U>(_ from: UnsafeMutablePointer<U>)
    /// Convert from a `UnsafePointer` of a different type.
    ///
    /// This is a fundamentally unsafe conversion.
    public init<U>(_ from: UnsafePointer<U>)
    /// Create an instance initialized with `nil`.
    public init(nilLiteral: ())
    /// Access the underlying raw memory, getting values.
    public var memory: Memory { get }
    public subscript (i: Int) -> Memory { get }
    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    public var hashValue: Int { get }
    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    public func successor() -> UnsafePointer<Memory>
    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    public func predecessor() -> UnsafePointer<Memory>
    public func distanceTo(x: UnsafePointer<Memory>) -> Int
    public func advancedBy(n: Int) -> UnsafePointer<Memory>
}

extension UnsafePointer : CustomDebugStringConvertible {
    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String { get }
}

extension UnsafePointer : _Reflectable {
}

extension UnsafePointer : CVarArgType {
}

/// A set of common requirements for Swift's unsigned integer types.
public protocol UnsignedIntegerType : _DisallowMixedSignArithmetic, IntegerType {
    /// Represent this number using Swift's widest native unsigned
    /// integer type.
    @warn_unused_result
    public func toUIntMax() -> UIntMax
    /// Convert from Swift's widest unsigned integer type, trapping on
    /// overflow.
    public init(_: UIntMax)
}

/// An object that can manage the lifetime of storage backing a
/// `CVaListPointer`.
final public class VaListBuilder {
}

/// The empty tuple type.
///
/// This is the default return type of functions for which no explicit
/// return type is specified.
public typealias Void = ()

/// A generator for `Zip2Sequence`.
public struct Zip2Generator<Generator1 : GeneratorType, Generator2 : GeneratorType> : GeneratorType {
    /// The type of element returned by `next()`.
    public typealias Element = (Generator1.Element, Generator2.Element)
    /// Construct around a pair of underlying generators.
    public init(_ generator1: Generator1, _ generator2: Generator2)
    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: `next()` has not been applied to a copy of `self`
    ///   since the copy was made, and no preceding call to `self.next()`
    ///   has returned `nil`.
    public mutating func next() -> (Generator1.Element, Generator2.Element)?
}

/// A sequence of pairs built out of two underlying sequences, where
/// the elements of the `i`th pair are the `i`th elements of each
/// underlying sequence.
public struct Zip2Sequence<Sequence1 : SequenceType, Sequence2 : SequenceType> : SequenceType {
    public typealias Stream1 = Sequence1.Generator
    public typealias Stream2 = Sequence2.Generator
    /// A type whose instances can produce the elements of this
    /// sequence, in order.
    public typealias Generator = Zip2Generator<Sequence1.Generator, Sequence2.Generator>
    /// Construct an instance that makes pairs of elements from `sequence1` and
    /// `sequence2`.
    public init(_ sequence1: Sequence1, _ sequence2: Sequence2)
    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    public func generate() -> Zip2Generator<Sequence1.Generator, Sequence2.Generator>
}

public func ^(lhs: UInt, rhs: UInt) -> UInt

public func ^(lhs: UInt8, rhs: UInt8) -> UInt8

public func ^(lhs: Int8, rhs: Int8) -> Int8

public func ^(lhs: UInt16, rhs: UInt16) -> UInt16

public func ^(lhs: Int16, rhs: Int16) -> Int16

public func ^(lhs: UInt32, rhs: UInt32) -> UInt32

public func ^(lhs: Int32, rhs: Int32) -> Int32

public func ^(lhs: UInt64, rhs: UInt64) -> UInt64

public func ^(lhs: Int64, rhs: Int64) -> Int64

public func ^(lhs: Int, rhs: Int) -> Int

public func ^=(inout lhs: Int, rhs: Int)

public func ^=(inout lhs: UInt64, rhs: UInt64)

public func ^=(inout lhs: UInt16, rhs: UInt16)

public func ^=(inout lhs: Int8, rhs: Int8)

public func ^=(inout lhs: UInt8, rhs: UInt8)

@warn_unused_result
public func ^=<T : BitwiseOperationsType>(inout lhs: T, rhs: T)

public func ^=(inout lhs: UInt32, rhs: UInt32)

public func ^=(inout lhs: Int64, rhs: Int64)

public func ^=(inout lhs: Int32, rhs: Int32)

public func ^=(inout lhs: UInt, rhs: UInt)

public func ^=(inout lhs: Int16, rhs: Int16)

/// The underlying buffer for an ArrayType conforms to
/// `_ArrayBufferType`.  This buffer does not provide value semantics.
public protocol _ArrayBufferType : MutableCollectionType {
    /// The type of elements stored in the buffer.
    typealias Element
    /// Create an empty buffer.
    public init()
    /// Adopt the entire buffer, presenting it at the provided `startIndex`.
    public init(_ buffer: _ContiguousArrayBuffer<Self.Element>, shiftedToStartIndex: Int)
    public subscript (index: Int) -> Self.Element { get nonmutating set }
    /// If this buffer is backed by a uniquely-referenced mutable
    /// `_ContiguousArrayBuffer` that can be grown in-place to allow the `self`
    /// buffer store `minimumCapacity` elements, returns that buffer.
    /// Otherwise, returns nil.
    ///
    /// - Note: The result's firstElementAddress may not match ours, if we are a
    ///   _SliceBuffer.
    ///
    /// - Note: This function must remain mutating; otherwise the buffer
    ///   may acquire spurious extra references, which will cause
    ///   unnecessary reallocation.
    @warn_unused_result
    public mutating func requestUniqueMutableBackingBuffer(minimumCapacity: Int) -> _ContiguousArrayBuffer<Self.Element>?
    /// Returns true iff this buffer is backed by a uniquely-referenced mutable
    /// _ContiguousArrayBuffer.
    ///
    /// - Note: This function must remain mutating; otherwise the buffer
    ///   may acquire spurious extra references, which will cause
    ///   unnecessary reallocation.
    @warn_unused_result
    public mutating func isMutableAndUniquelyReferenced() -> Bool
    /// If this buffer is backed by a `_ContiguousArrayBuffer`
    /// containing the same number of elements as `self`, return it.
    /// Otherwise, return `nil`.
    @warn_unused_result
    public func requestNativeBuffer() -> _ContiguousArrayBuffer<Self.Element>?
    /// Replace the given `subRange` with the first `newCount` elements of
    /// the given collection.
    ///
    /// - Requires: This buffer is backed by a uniquely-referenced
    /// `_ContiguousArrayBuffer`.
    public mutating func replace<C : CollectionType where C.Generator.Element == Element>(subRange subRange: Range<Int>, with newCount: Int, elementsOf newValues: C)
    public subscript (subRange: Range<Int>) -> _SliceBuffer<Self.Element> { get }
    /// Call `body(p)`, where `p` is an `UnsafeBufferPointer` over the
    /// underlying contiguous storage.  If no such storage exists, it is
    /// created on-demand.
    public func withUnsafeBufferPointer<R>(@noescape body: (UnsafeBufferPointer<Self.Element>) throws -> R) rethrows -> R
    /// Call `body(p)`, where `p` is an `UnsafeMutableBufferPointer`
    /// over the underlying contiguous storage.
    ///
    /// - Requires: Such contiguous storage exists or the buffer is empty.
    public mutating func withUnsafeMutableBufferPointer<R>(@noescape body: (UnsafeMutableBufferPointer<Self.Element>) throws -> R) rethrows -> R
    /// The number of elements the buffer stores.
    public var count: Int { get set }
    /// The number of elements the buffer can store without reallocation.
    public var capacity: Int { get }
    /// An object that keeps the elements stored in this buffer alive.
    public var owner: AnyObject { get }
    /// If the elements are stored contiguously, a pointer to the first
    /// element. Otherwise, `nil`.
    public var firstElementAddress: UnsafeMutablePointer<Self.Element> { get }
    /// Return a base address to which you can add an index `i` to get the address
    /// of the corresponding element at `i`.
    public var subscriptBaseAddress: UnsafeMutablePointer<Self.Element> { get }
    /// A value that identifies the storage used by the buffer.  Two
    /// buffers address the same elements when they have the same
    /// identity and count.
    public var identity: UnsafePointer<Void> { get }
    public var startIndex: Int { get }
}

extension _ArrayBufferType {
    public var subscriptBaseAddress: UnsafeMutablePointer<Self.Element> { get }
    public mutating func replace<C : CollectionType where C.Generator.Element == Element>(subRange subRange: Range<Int>, with newCount: Int, elementsOf newValues: C)
}

public protocol _ArrayType : RangeReplaceableCollectionType, MutableSliceable, ArrayLiteralConvertible {
    /// Construct an array of `count` elements, each initialized to `repeatedValue`.
    public init(count: Int, repeatedValue: Self.Generator.Element)
    /// The number of elements the Array stores.
    public var count: Int { get }
    /// The number of elements the Array can store without reallocation.
    public var capacity: Int { get }
    /// `true` if and only if the Array is empty.
    public var isEmpty: Bool { get }
    public subscript (index: Int) -> Self.Generator.Element { get set }
    /// Reserve enough space to store minimumCapacity elements.
    ///
    /// - Postcondition: `capacity >= minimumCapacity` and the array has
    ///   mutable contiguous storage.
    ///
    /// - Complexity: O(`count`).
    public mutating func reserveCapacity(minimumCapacity: Int)
    /// Operator form of `appendContentsOf`.
    public func +=<S : SequenceType where S.Generator.Element == Generator.Element>(inout lhs: Self, rhs: S)
    /// Insert `newElement` at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count`).
    ///
    /// - Requires: `atIndex <= count`.
    public mutating func insert(newElement: Self.Generator.Element, atIndex i: Int)
    /// Remove and return the element at the given index.
    ///
    /// - returns: The removed element.
    ///
    /// - Complexity: Worst case O(N).
    ///
    /// - Requires: `count > index`.
    public mutating func removeAtIndex(index: Int) -> Self.Generator.Element
    public init(_ buffer: Self._Buffer)
}

/// Some types require alignment greater than Int on some architectures.
public protocol _CVarArgAlignedType : CVarArgType {
}

/// Floating point types need to be passed differently on x86_64
/// systems.  CoreGraphics uses this to make CGFloat work properly.
public protocol _CVarArgPassedAsDouble : CVarArgType {
}

/// Effectively a proxy for NSString that doesn't mention it by
/// name.  NSString's conformance to this protocol is declared in
/// Foundation.
@objc public protocol _CocoaStringType {
}

public protocol _CollectionWrapperType : _SequenceWrapperType {
    typealias Base : CollectionType
    typealias Index : ForwardIndexType = Self.Base.Index
}

/// Conforming types can be initialized with color literals (e.g.
/// `[#Color(colorLiteralRed: 1, blue: 0, green: 0, alpha: 1)#]`).
public protocol _ColorLiteralConvertible {
    public init(colorLiteralRed: Float, green: Float, blue: Float, alpha: Float)
}

/// A container is destructor safe if whether it may store to memory on
/// destruction only depends on its type parameters.
/// For example, whether `Array<T>` may store to memory on destruction depends
/// only on `T`.
/// If `T` is an `Int` we know the `Array<Int>` does not store to memory during
/// destruction. If `T` is an arbitrary class `Array<MemoryUnsafeDestructorClass>`
/// then the compiler will deduce may store to memory on destruction because
/// `MemoryUnsafeDestructorClass`'s destructor may store to memory on destruction.
public protocol _DestructorSafeContainer {
}

/// This protocol is an implementation detail of `UnsignedIntegerType`;
/// do not use it directly.
public protocol _DisallowMixedSignArithmetic : _IntegerType {
}

/// Conforming types can be initialized with strings (e.g.
/// `[#FileReference(fileReferenceLiteral: "resource.txt")#]`).
public protocol _FileReferenceLiteralConvertible {
    public init(fileReferenceLiteral: String)
}

/// Conforming types can be initialized with image literals (e.g.
/// `[#Image(imageLiteral: "hi.png")#]`).
public protocol _ImageLiteralConvertible {
    public init(imageLiteral: String)
}

/// This protocol is an implementation detail of `ForwardIndexType`; do
/// not use it directly.
///
/// Its requirements are inherited by `ForwardIndexType` and thus must
/// be satisfied by types conforming to that protocol.
public protocol _Incrementable : Equatable {
    /// Return the next consecutive value in a discrete sequence of
    /// `Self` values.
    ///
    /// - Requires: `self` has a well-defined successor.
    @warn_unused_result
    public func successor() -> Self
}



/// This protocol is an implementation detail of `IntegerArithmeticType`; do
/// not use it directly.
///
/// Its requirements are inherited by `IntegerArithmeticType` and thus must
/// be satisfied by types conforming to that protocol.
public protocol _IntegerArithmeticType {
    /// Add `lhs` and `rhs`, returning a result and a `Bool` that is
    /// true iff the operation caused an arithmetic overflow.
    public static func addWithOverflow(lhs: Self, _ rhs: Self) -> (Self, overflow: Bool)
    /// Subtract `lhs` and `rhs`, returning a result and a `Bool` that is
    /// true iff the operation caused an arithmetic overflow.
    public static func subtractWithOverflow(lhs: Self, _ rhs: Self) -> (Self, overflow: Bool)
    /// Multiply `lhs` and `rhs`, returning a result and a `Bool` that is
    /// true iff the operation caused an arithmetic overflow.
    public static func multiplyWithOverflow(lhs: Self, _ rhs: Self) -> (Self, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning a result and a `Bool` that is
    /// true iff the operation caused an arithmetic overflow.
    public static func divideWithOverflow(lhs: Self, _ rhs: Self) -> (Self, overflow: Bool)
    /// Divide `lhs` and `rhs`, returning the remainder and a `Bool` that is
    /// true iff the operation caused an arithmetic overflow.
    public static func remainderWithOverflow(lhs: Self, _ rhs: Self) -> (Self, overflow: Bool)
}

/// This protocol is an implementation detail of `IntegerType`; do
/// not use it directly.
public protocol _IntegerType : IntegerLiteralConvertible, CustomStringConvertible, Hashable, IntegerArithmeticType, BitwiseOperationsType, _Incrementable {
}

/// The type returned by `_reflect(x)`; supplies an API for runtime
/// reflection on `x`.
public protocol _MirrorType {
    /// The instance being reflected.
    public var value: Any { get }
    /// Identical to `value.dynamicType`.
    public var valueType: Any.Type { get }
    /// A unique identifier for `value` if it is a class instance; `nil`
    /// otherwise.
    public var objectIdentifier: ObjectIdentifier? { get }
    /// The count of `value`'s logical children.
    public var count: Int { get }
    public subscript (i: Int) -> (String, _MirrorType) { get }
    /// A string description of `value`.
    public var summary: String { get }
    /// A rich representation of `value` for an IDE, or `nil` if none is supplied.
    public var quickLookObject: PlaygroundQuickLook? { get }
    /// How `value` should be presented in an IDE.
    public var disposition: _MirrorDisposition { get }
}



/// A shadow for the "core operations" of NSArray.
///
/// Covers a set of operations everyone needs to implement in order to
/// be a useful `NSArray` subclass.
@objc public protocol _NSArrayCoreType : _NSCopyingType, _NSFastEnumerationType {
    public func objectAtIndex(index: Int) -> AnyObject
    public func getObjects(_: UnsafeMutablePointer<AnyObject>, range: _SwiftNSRange)
    public func countByEnumeratingWithState(state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>, objects: UnsafeMutablePointer<AnyObject>, count: Int) -> Int
    public var count: Int { get }
}

/// A shadow for the `NSCopying` protocol.
@objc public protocol _NSCopyingType : _ShadowProtocol {
    public func copyWithZone(zone: _SwiftNSZone) -> AnyObject
}

/// A shadow for the "core operations" of NSDictionary.
///
/// Covers a set of operations everyone needs to implement in order to
/// be a useful `NSDictionary` subclass.
@objc public protocol _NSDictionaryCoreType : _NSCopyingType, _NSFastEnumerationType {
    public init(objects: UnsafePointer<AnyObject?>, forKeys: UnsafePointer<Void>, count: Int)
    public var count: Int { get }
    public func objectForKey(aKey: AnyObject) -> AnyObject?
    public func keyEnumerator() -> _NSEnumeratorType
    public func copyWithZone(zone: _SwiftNSZone) -> AnyObject
    public func getObjects(objects: UnsafeMutablePointer<AnyObject>, andKeys keys: UnsafeMutablePointer<AnyObject>)
    public func countByEnumeratingWithState(state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>, objects: UnsafeMutablePointer<AnyObject>, count: Int) -> Int
}

/// A shadow for the API of `NSDictionary` we will use in the core
/// stdlib.
///
/// `NSDictionary` operations, in addition to those on
/// `_NSDictionaryCoreType`, that we need to use from the core stdlib.
/// Distinct from `_NSDictionaryCoreType` because we don't want to be
/// forced to implement operations that `NSDictionary` already
/// supplies.
@objc public protocol _NSDictionaryType : _NSDictionaryCoreType {
    public func getObjects(objects: UnsafeMutablePointer<AnyObject>, andKeys keys: UnsafeMutablePointer<AnyObject>)
}

/// A shadow for the `NSEnumerator` class.
@objc public protocol _NSEnumeratorType : _ShadowProtocol {
    public init()
    public func nextObject() -> AnyObject?
}

/// A shadow for the `NSFastEnumeration` protocol.
@objc public protocol _NSFastEnumerationType : _ShadowProtocol {
    public func countByEnumeratingWithState(state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>, objects: UnsafeMutablePointer<AnyObject>, count: Int) -> Int
}

/// A shadow for the "core operations" of NSSet.
///
/// Covers a set of operations everyone needs to implement in order to
/// be a useful `NSSet` subclass.
@objc public protocol _NSSetCoreType : _NSCopyingType, _NSFastEnumerationType {
    public init(objects: UnsafePointer<AnyObject?>, count: Int)
    public var count: Int { get }
    public func member(object: AnyObject) -> AnyObject?
    public func objectEnumerator() -> _NSEnumeratorType
    public func copyWithZone(zone: _SwiftNSZone) -> AnyObject
    public func countByEnumeratingWithState(state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>, objects: UnsafeMutablePointer<AnyObject>, count: Int) -> Int
}

/// A shadow for the API of NSSet we will use in the core
/// stdlib.
///
/// `NSSet` operations, in addition to those on
/// `_NSSetCoreType`, that we need to use from the core stdlib.
/// Distinct from `_NSSetCoreType` because we don't want to be
/// forced to implement operations that `NSSet` already
/// supplies.
@objc public protocol _NSSetType : _NSSetCoreType {
}

@objc public protocol _NSStringCoreType : _NSCopyingType, _NSFastEnumerationType {
    public func length() -> Int
    public func characterAtIndex(index: Int) -> UInt16
}

/// A Swift Array or Dictionary of types conforming to
/// `_ObjectiveCBridgeable` can be passed to Objective-C as an NSArray or
/// NSDictionary, respectively.  The elements of the resulting NSArray
/// or NSDictionary will be the result of calling `_bridgeToObjectiveC`
/// on each elmeent of the source container.
public protocol _ObjectiveCBridgeable {
}

/// A stdlib-internal protocol modeled by the intrinsic pointer types,
/// UnsafeMutablePointer, UnsafePointer, and
/// AutoreleasingUnsafeMutablePointer.
public protocol _PointerType {
}

/// Used to force conformers of RandomAccessIndexType to implement
/// `advancedBy` methods and `distanceTo`.
public protocol _RandomAccessAmbiguity {
    typealias Distance : _SignedIntegerType = Int
}

extension _RandomAccessAmbiguity {
    @warn_unused_result
    public func advancedBy(n: Self.Distance) -> Self
}

/// Customizes the result of `_reflect(x)`, where `x` is a conforming
/// type.
public protocol _Reflectable {
}

public protocol _ReverseCollectionType : CollectionType {
    typealias Index : ReverseIndexType
    typealias Base : CollectionType
}

extension _ReverseCollectionType where Self : CollectionType, Self.Index.Base == Self.Base.Index {
    public var startIndex: Self.Index { get }
    public var endIndex: Self.Index { get }
    public subscript (position: Self.Index) -> Self.Base.Generator.Element { get }
}

/// A type that is just a wrapper over some base Sequence
public protocol _SequenceWrapperType {
    typealias Base : SequenceType
    typealias Generator : GeneratorType = Self.Base.Generator
}

@objc public protocol _ShadowProtocol {
}

/// This protocol is an implementation detail of `SignedIntegerType`;
/// do not use it directly.
public protocol _SignedIntegerType : _IntegerType, SignedNumberType {
    /// Represent this number using Swift's widest native signed integer
    /// type.
    @warn_unused_result
    public func toIntMax() -> IntMax
    /// Convert from Swift's widest signed integer type, trapping on
    /// overflow.
    public init(_: IntMax)
}

public protocol _SinkType {
}

/// This protocol is an implementation detail of `Strideable`; do
/// not use it directly.
public protocol _Strideable {
    /// A type that can represent the distance between two values of `Self`.
    typealias Stride : SignedNumberType
    /// Returns a stride `x` such that `self.advancedBy(x)` approximates
    /// `other`.
    ///
    /// - Complexity: O(1).
    ///
    /// - SeeAlso: `RandomAccessIndexType`'s `distanceTo`, which provides a
    ///   stronger semantic guarantee.
    @warn_unused_result
    public func distanceTo(other: Self) -> Self.Stride
    /// Returns a `Self` `x` such that `self.distanceTo(x)` approximates
    /// `n`.
    ///
    /// - Complexity: O(1).
    ///
    /// - SeeAlso: `RandomAccessIndexType`'s `advancedBy`, which
    ///   provides a stronger semantic guarantee.
    @warn_unused_result
    public func advancedBy(n: Self.Stride) -> Self
}

/// Instances of conforming types are used in internal `String`
/// representation.
public protocol _StringElementType {
}

/// Return the absolute value of `x`.
///
/// Concrete instances of `SignedNumberType` can specialize this
/// function by conforming to `AbsoluteValuable`.
public func abs<T : SignedNumberType>(x: T) -> T

/// Returns the minimum memory alignment of `T`.
@warn_unused_result
public func alignof<T>(_: T.Type) -> Int

/// Returns the minimum memory alignment of `T`.
@warn_unused_result
public func alignofValue<T>(_: T) -> Int

/// Return a `GeneratorType` instance that wraps `base` but whose type
/// depends only on the type of `G.Element`.
///
/// Example:
///
///     func countStrings() -> AnyGenerator<String> {
///       let lazyStrings = (0..<10).lazy.map { String($0) }
///
///       // This is a really complicated type of no interest to our
///       // clients.
///       let g: MapSequenceGenerator<RangeGenerator<Int>, String>
///         = lazyStrings.generate()
///       return anyGenerator(g)
///     }
public func anyGenerator<G : GeneratorType>(base: G) -> AnyGenerator<G.Element>

/// Return a `GeneratorType` instance whose `next` method invokes
/// `body` and returns the result.
///
/// Example:
///
///     var x = 7
///     let g = anyGenerator { x < 15 ? x++ : nil }
///     let a = Array(g) // [ 7, 8, 9, 10, 11, 12, 13, 14 ]
@warn_unused_result
public func anyGenerator<Element>(body: () -> Element?) -> AnyGenerator<Element>

/// Traditional C-style assert with an optional message.
///
/// Use this function for internal sanity checks that are active
/// during testing but do not impact performance of shipping code.
/// To check for invalid usage in Release builds; see `precondition`.
///
/// * In playgrounds and -Onone builds (the default for Xcode's Debug
///   configuration): if `condition` evaluates to false, stop program
///   execution in a debuggable state after printing `message`.
///
/// * In -O builds (the default for Xcode's Release configuration),
///   `condition` is not evaluated, and there are no effects.
///
/// * In -Ounchecked builds, `condition` is not evaluated, but the
///   optimizer may assume that it *would* evaluate to `true`. Failure
///   to satisfy that assumption in -Ounchecked builds is a serious
///   programming error.
public func assert(@autoclosure condition: () -> Bool, @autoclosure _ message: () -> String = default, file: StaticString = default, line: UInt = default)

/// Indicate that an internal sanity check failed.
///
/// Use this function to stop the program, without impacting the
/// performance of shipping code, when control flow is not expected to
/// reach the call (e.g. in the `default` case of a `switch` where you
/// have knowledge that one of the other cases must be satisfied). To
/// protect code from invalid usage in Release builds; see
/// `preconditionFailure`.
///
/// * In playgrounds and -Onone builds (the default for Xcode's Debug
///   configuration) stop program execution in a debuggable state
///   after printing `message`.
///
/// * In -O builds, has no effect.
///
/// * In -Ounchecked builds, the optimizer may assume that this
///   function will never be called. Failure to satisfy that assumption
///   is a serious programming error.
public func assertionFailure(@autoclosure message: () -> String = default, file: StaticString = default, line: UInt = default)

/// Writes the textual representations of `items` most suitable for
/// debugging, separated by `separator` and terminated by
/// `terminator`, into the standard output.
///
/// The textual representations are obtained for each `item` via
/// the expression `String(reflecting: item)`.
///
/// - Note: to print without a trailing newline, pass `terminator: ""`
///
/// - SeeAlso: `print`, Streamable`, `CustomStringConvertible`,
///   `CustomDebugStringConvertible`
public func debugPrint(items: Any..., separator: String = default, terminator: String = default)

/// Writes the textual representations of `items` most suitable for
/// debugging, separated by `separator` and terminated by
/// `terminator`, into `output`.
///
/// The textual representations are obtained for each `item` via
/// the expression `String(reflecting: item)`.
///
/// - Note: to print without a trailing newline, pass `terminator: ""`
///
/// - SeeAlso: `print`, Streamable`, `CustomStringConvertible`,
///   `CustomDebugStringConvertible`
public func debugPrint<Target : OutputStreamType>(items: Any..., separator: String = default, terminator: String = default, inout toStream output: Target)

/// Dump an object's contents using its mirror to standard output.
public func dump<T>(x: T, name: String? = default, indent: Int = default, maxDepth: Int = default, maxItems: Int = default) -> T

/// Dump an object's contents using its mirror to the specified output stream.
public func dump<T, TargetStream : OutputStreamType>(x: T, inout _ targetStream: TargetStream, name: String? = default, indent: Int = default, maxDepth: Int = default, maxItems: Int = default) -> T

/// Unconditionally print a `message` and stop execution.
@noreturn public func fatalError(@autoclosure message: () -> String = default, file: StaticString = default, line: UInt = default)

/// Returns a `CVaListPointer` built from `args` that's backed by
/// autoreleased storage.
///
/// - Warning: This function is best avoided in favor of
///   `withVaList`, but occasionally (i.e. in a `class` initializer) you
///   may find that the language rules don't allow you to use
/// `withVaList` as intended.
@warn_unused_result
public func getVaList(args: [CVarArgType]) -> CVaListPointer

/// Returns `true` iff `object` is a non-`@objc` class instance with a single
/// strong reference.
///
/// * Does *not* modify `object`; the use of `inout` is an
///   implementation artifact.
/// * Weak references do not affect the result of this function.
///
/// Useful for implementing the copy-on-write optimization for the
/// deep storage of value types:
///
///     mutating func modifyMe(arg: X) {
///       if isUniquelyReferenced(&myStorage) {
///         myStorage.modifyInPlace(arg)
///       }
///       else {
///         myStorage = myStorage.createModified(arg)
///       }
///     }
///
/// This function is safe to use for `mutating` functions in
/// multithreaded code because a false positive would imply that there
/// is already a user-level data race on the value being mutated.
public func isUniquelyReferenced<T : NonObjectiveCBase>(inout object: T) -> Bool

/// Returns `true` iff `object` is a non-`@objc` class instance with
/// a single strong reference.
///
/// * Does *not* modify `object`; the use of `inout` is an
///   implementation artifact.
/// * If `object` is an Objective-C class instance, returns `false`.
/// * Weak references do not affect the result of this function.
///
/// Useful for implementing the copy-on-write optimization for the
/// deep storage of value types:
///
///     mutating func modifyMe(arg: X) {
///       if isUniquelyReferencedNonObjC(&myStorage) {
///         myStorage.modifyInPlace(arg)
///       }
///       else {
///         myStorage = self.createModified(myStorage, arg)
///       }
///     }
///
/// This function is safe to use for `mutating` functions in
/// multithreaded code because a false positive would imply that there
/// is already a user-level data race on the value being mutated.
public func isUniquelyReferencedNonObjC<T : AnyObject>(inout object: T) -> Bool

/// Returns `true` iff `object` is a non-`@objc` class instance with
/// a single strong reference.
///
/// * Does *not* modify `object`; the use of `inout` is an
///   implementation artifact.
/// * If `object` is an Objective-C class instance, returns `false`.
/// * Weak references do not affect the result of this function.
///
/// Useful for implementing the copy-on-write optimization for the
/// deep storage of value types:
///
///     mutating func modifyMe(arg: X) {
///       if isUniquelyReferencedNonObjC(&myStorage) {
///         myStorage.modifyInPlace(arg)
///       }
///       else {
///         myStorage = self.createModified(myStorage, arg)
///       }
///     }
///
/// This function is safe to use for `mutating` functions in
/// multithreaded code because a false positive would imply that there
/// is already a user-level data race on the value being mutated.
public func isUniquelyReferencedNonObjC<T : AnyObject>(inout object: T?) -> Bool

/// Returns the greatest argument passed.
@warn_unused_result
public func max<T : Comparable>(x: T, _ y: T, _ z: T, _ rest: T...) -> T

/// Returns the greater of `x` and `y`.
@warn_unused_result
public func max<T : Comparable>(x: T, _ y: T) -> T

/// Returns the lesser of `x` and `y`.
@warn_unused_result
public func min<T : Comparable>(x: T, _ y: T) -> T

/// Returns the least argument passed.
@warn_unused_result
public func min<T : Comparable>(x: T, _ y: T, _ z: T, _ rest: T...) -> T

/// Convert `x` to type `U`, trapping on overflow in -Onone and -O
/// builds.
///
/// Typically used to do conversion to any contextually-deduced
/// integer type:
///
///     func f(x: Int32) {}
///     func g(x: Int64) { f(numericCast(x)) }
@warn_unused_result
public func numericCast<T : _SignedIntegerType, U : _SignedIntegerType>(x: T) -> U

/// Convert `x` to type `U`, trapping on overflow in -Onone and -O
/// builds.
///
/// Typically used to do conversion to any contextually-deduced
/// integer type:
///
///     func f(x: UInt32) {}
///     func g(x: UInt64) { f(numericCast(x)) }
@warn_unused_result
public func numericCast<T : UnsignedIntegerType, U : UnsignedIntegerType>(x: T) -> U

/// Convert `x` to type `U`, trapping on overflow in -Onone and -O
/// builds.
///
/// Typically used to do conversion to any contextually-deduced
/// integer type:
///
///     func f(x: UInt32) {}
///     func g(x: Int64) { f(numericCast(x)) }
@warn_unused_result
public func numericCast<T : _SignedIntegerType, U : UnsignedIntegerType>(x: T) -> U

/// Convert `x` to type `U`, trapping on overflow in -Onone and -O
/// builds.
///
/// Typically used to do conversion to any contextually-deduced
/// integer type:
///
///     func f(x: Int32) {}
///     func g(x: UInt64) { f(numericCast(x)) }
@warn_unused_result
public func numericCast<T : UnsignedIntegerType, U : _SignedIntegerType>(x: T) -> U

/// Check a necessary condition for making forward progress.
///
/// Use this function to detect conditions that must prevent the
/// program from proceeding even in shipping code.
///
/// * In playgrounds and -Onone builds (the default for Xcode's Debug
///   configuration): if `condition` evaluates to false, stop program
///   execution in a debuggable state after printing `message`.
///
/// * In -O builds (the default for Xcode's Release configuration):
///   if `condition` evaluates to false, stop program execution.
///
/// * In -Ounchecked builds, `condition` is not evaluated, but the
///   optimizer may assume that it *would* evaluate to `true`. Failure
///   to satisfy that assumption in -Ounchecked builds is a serious
///   programming error.
public func precondition(@autoclosure condition: () -> Bool, @autoclosure _ message: () -> String = default, file: StaticString = default, line: UInt = default)

/// Indicate that a precondition was violated.
///
/// Use this function to stop the program when control flow can only
/// reach the call if your API was improperly used.
///
/// * In playgrounds and -Onone builds (the default for Xcode's Debug
///   configuration), stop program execution in a debuggable state
///   after printing `message`.
///
/// * In -O builds (the default for Xcode's Release configuration),
///   stop program execution.
///
/// * In -Ounchecked builds, the optimizer may assume that this
///   function will never be called. Failure to satisfy that assumption
///   is a serious programming error.
@noreturn public func preconditionFailure(@autoclosure message: () -> String = default, file: StaticString = default, line: UInt = default)

/// Writes the textual representations of `items`, separated by
/// `separator` and terminated by `terminator`, into the standard
/// output.
///
/// The textual representations are obtained for each `item` via
/// the expression `String(item)`.
///
/// - Note: to print without a trailing newline, pass `terminator: ""`
///
/// - SeeAlso: `debugPrint`, Streamable`, `CustomStringConvertible`,
///   `CustomDebugStringConvertible`
public func print(items: Any..., separator: String = default, terminator: String = default)

/// Writes the textual representations of `items`, separated by
/// `separator` and terminated by `terminator`, into `output`.
///
/// The textual representations are obtained for each `item` via
/// the expression `String(item)`.
///
/// - Note: to print without a trailing newline, pass `terminator: ""`
///
/// - SeeAlso: `debugPrint`, Streamable`, `CustomStringConvertible`,
///   `CustomDebugStringConvertible`
public func print<Target : OutputStreamType>(items: Any..., separator: String = default, terminator: String = default, inout toStream output: Target)

/// Returns `Character`s read from standard input through the end of the
/// current line or until EOF is reached, or `nil` if EOF has already been
/// reached.
///
/// If `stripNewline` is `true`, newline characters and character
/// combinations will be stripped from the result.  This is the default.
///
/// Standard input is interpreted as `UTF-8`.  Invalid bytes
/// will be replaced by Unicode [replacement characters](http://en.wikipedia.org/wiki/Specials_(Unicode_block)#Replacement_character).
@warn_unused_result
public func readLine(stripNewline stripNewline: Bool = default) -> String?

/// Returns the contiguous memory footprint of `T`.
///
/// Does not include any dynamically-allocated or "remote" storage.
/// In particular, `sizeof(X.self)`, when `X` is a class type, is the
/// same regardless of how many stored properties `X` has.
@warn_unused_result
public func sizeof<T>(_: T.Type) -> Int

/// Returns the contiguous memory footprint of  `T`.
///
/// Does not include any dynamically-allocated or "remote" storage.
/// In particular, `sizeof(a)`, when `a` is a class instance, is the
/// same regardless of how many stored properties `a` has.
@warn_unused_result
public func sizeofValue<T>(_: T) -> Int

/// Returns the least possible interval between distinct instances of
/// `T` in memory.  The result is always positive.
@warn_unused_result
public func strideof<T>(_: T.Type) -> Int

/// Returns the least possible interval between distinct instances of
/// `T` in memory.  The result is always positive.
@warn_unused_result
public func strideofValue<T>(_: T) -> Int

/// Exchange the values of `a` and `b`.
public func swap<T>(inout a: T, inout _ b: T)

/// Translate `input`, in the given `InputEncoding`, into `output`, in
/// the given `OutputEncoding`.
///
/// - parameter stopOnError: Causes encoding to stop when an encoding
///   error is detected in `input`, if `true`.  Otherwise, U+FFFD
///   replacement characters are inserted for each detected error.
public func transcode<Input : GeneratorType, InputEncoding : UnicodeCodecType, OutputEncoding : UnicodeCodecType where InputEncoding.CodeUnit == Input.Element>(inputEncoding: InputEncoding.Type, _ outputEncoding: OutputEncoding.Type, _ input: Input, _ output: (OutputEncoding.CodeUnit) -> (), stopOnError: Bool) -> Bool

/// Returns an `UnsafePointer` to the storage used for `object`.  There's
/// not much you can do with this other than use it to identify the
/// object.
@warn_unused_result
public func unsafeAddressOf(object: AnyObject) -> UnsafePointer<Void>

/// Returns the the bits of `x`, interpreted as having type `U`.
///
/// - Warning: Breaks the guarantees of Swift's type system; use
///   with extreme care.  There's almost always a better way to do
///   anything.
///
@warn_unused_result
public func unsafeBitCast<T, U>(x: T, _: U.Type) -> U

/// - returns: `x as T`.
///
/// - Requires: `x is T`.  In particular, in -O builds, no test is
///   performed to ensure that `x` actually has dynamic type `T`.
///
/// - Warning: Trades safety for performance.  Use `unsafeDowncast`
///   only when `x as T` has proven to be a performance problem and you
///   are confident that, always, `x is T`.  It is better than an
///   `unsafeBitCast` because it's more restrictive, and because
///   checking is still performed in debug builds.
@warn_unused_result
public func unsafeDowncast<T : AnyObject>(x: AnyObject) -> T

/// - Returns: `nonEmpty!`.
///
/// - Requires: `nonEmpty != nil`.  In particular, in -O builds, no test
///   is performed to ensure that `nonEmpty` actually is non-nil.
///
/// - Warning: Trades safety for performance.  Use `unsafeUnwrap`
///   only when `nonEmpty!` has proven to be a performance problem and
///   you are confident that, always, `nonEmpty != nil`.  It is better
///   than an `unsafeBitCast` because it's more restrictive, and
///   because checking is still performed in debug builds.
@warn_unused_result
public func unsafeUnwrap<T>(nonEmpty: T?) -> T

/// Evaluate `f(x)` and return its result, ensuring that `x` is not
/// destroyed before f returns.
public func withExtendedLifetime<T, Result>(x: T, @noescape _ f: T throws -> Result) rethrows -> Result

/// Evaluate `f()` and return its result, ensuring that `x` is not
/// destroyed before f returns.
public func withExtendedLifetime<T, Result>(x: T, @noescape _ f: () throws -> Result) rethrows -> Result

/// Invokes `body` with an `UnsafeMutablePointer` to `arg` and returns the
/// result. Useful for calling Objective-C APIs that take "in/out"
/// parameters (and default-constructible "out" parameters) by pointer.
public func withUnsafeMutablePointer<T, Result>(inout arg: T, @noescape _ body: UnsafeMutablePointer<T> throws -> Result) rethrows -> Result

/// Like `withUnsafeMutablePointer`, but passes pointers to `arg0`, `arg1`,
/// and `arg2`.
public func withUnsafeMutablePointers<A0, A1, A2, Result>(inout arg0: A0, inout _ arg1: A1, inout _ arg2: A2, @noescape _ body: (UnsafeMutablePointer<A0>, UnsafeMutablePointer<A1>, UnsafeMutablePointer<A2>) throws -> Result) rethrows -> Result

/// Like `withUnsafeMutablePointer`, but passes pointers to `arg0` and `arg1`.
public func withUnsafeMutablePointers<A0, A1, Result>(inout arg0: A0, inout _ arg1: A1, @noescape _ body: (UnsafeMutablePointer<A0>, UnsafeMutablePointer<A1>) throws -> Result) rethrows -> Result

/// Invokes `body` with an `UnsafePointer` to `arg` and returns the
/// result. Useful for calling Objective-C APIs that take "in/out"
/// parameters (and default-constructible "out" parameters) by pointer.
public func withUnsafePointer<T, Result>(inout arg: T, @noescape _ body: UnsafePointer<T> throws -> Result) rethrows -> Result

/// Like `withUnsafePointer`, but passes pointers to `arg0` and `arg1`.
public func withUnsafePointers<A0, A1, Result>(inout arg0: A0, inout _ arg1: A1, @noescape _ body: (UnsafePointer<A0>, UnsafePointer<A1>) throws -> Result) rethrows -> Result

/// Like `withUnsafePointer`, but passes pointers to `arg0`, `arg1`,
/// and `arg2`.
public func withUnsafePointers<A0, A1, A2, Result>(inout arg0: A0, inout _ arg1: A1, inout _ arg2: A2, @noescape _ body: (UnsafePointer<A0>, UnsafePointer<A1>, UnsafePointer<A2>) throws -> Result) rethrows -> Result

/// Invoke `f` with a C `va_list` argument derived from `args`.
public func withVaList<R>(args: [CVarArgType], @noescape _ f: CVaListPointer -> R) -> R

/// Invoke `f` with a C `va_list` argument derived from `builder`.
public func withVaList<R>(builder: VaListBuilder, @noescape _ f: CVaListPointer -> R) -> R

/// A sequence of pairs built out of two underlying sequences, where
/// the elements of the `i`th pair are the `i`th elements of each
/// underlying sequence.
public func zip<Sequence1 : SequenceType, Sequence2 : SequenceType>(sequence1: Sequence1, _ sequence2: Sequence2) -> Zip2Sequence<Sequence1, Sequence2>

public func |(lhs: Int8, rhs: Int8) -> Int8

public func |(lhs: UInt8, rhs: UInt8) -> UInt8

public func |(lhs: Int16, rhs: Int16) -> Int16

public func |(lhs: UInt32, rhs: UInt32) -> UInt32

public func |(lhs: Int32, rhs: Int32) -> Int32

public func |(lhs: UInt64, rhs: UInt64) -> UInt64

public func |(lhs: Int64, rhs: Int64) -> Int64

public func |(lhs: UInt, rhs: UInt) -> UInt

public func |(lhs: Int, rhs: Int) -> Int

public func |(lhs: UInt16, rhs: UInt16) -> UInt16

public func |=(inout lhs: Int8, rhs: Int8)

public func |=(inout lhs: UInt8, rhs: UInt8)

@warn_unused_result
public func |=<T : BitwiseOperationsType>(inout lhs: T, rhs: T)

public func |=(inout lhs: Int, rhs: Int)

public func |=(inout lhs: UInt, rhs: UInt)

public func |=(inout lhs: Int64, rhs: Int64)

public func |=(inout lhs: UInt64, rhs: UInt64)

public func |=(inout lhs: Int32, rhs: Int32)

public func |=(inout lhs: UInt32, rhs: UInt32)

public func |=(inout lhs: Int16, rhs: Int16)

public func |=(inout lhs: UInt16, rhs: UInt16)

@warn_unused_result
public func ||<T : BooleanType>(lhs: T, @autoclosure rhs: () throws -> Bool) rethrows -> Bool

/// If `lhs` is `true`, return it.  Otherwise, evaluate `rhs` and
/// return its `boolValue`.
@warn_unused_result
public func ||<T : BooleanType, U : BooleanType>(lhs: T, @autoclosure rhs: () throws -> U) rethrows -> Bool

prefix public func ~(rhs: Int) -> Int

prefix public func ~(rhs: UInt) -> UInt

prefix public func ~(rhs: Int64) -> Int64

prefix public func ~(rhs: UInt64) -> UInt64

prefix public func ~(rhs: Int32) -> Int32

prefix public func ~(rhs: Int16) -> Int16

prefix public func ~(rhs: UInt16) -> UInt16

prefix public func ~(rhs: Int8) -> Int8

prefix public func ~(rhs: UInt8) -> UInt8

prefix public func ~(rhs: UInt32) -> UInt32

@warn_unused_result
public func ~=<I : ForwardIndexType where I : Comparable>(pattern: Range<I>, value: I) -> Bool

@warn_unused_result
public func ~=<T : Equatable>(a: T, b: T) -> Bool

@warn_unused_result
public func ~=<T>(lhs: _OptionalNilComparisonType, rhs: T?) -> Bool

/// Returns `true` iff `pattern` contains `value`.
@warn_unused_result
public func ~=<I : IntervalType>(pattern: I, value: I.Bound) -> Bool

