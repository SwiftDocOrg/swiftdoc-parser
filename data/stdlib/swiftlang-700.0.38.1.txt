extension _SwiftNSOperatingSystemVersion : Comparable, Equatable {
}

infix operator %= {
    associativity right
    precedence 90
    assignment
}

infix operator ~= {
    associativity none
    precedence 130
}

infix operator += {
    associativity right
    precedence 90
    assignment
}

infix operator -= {
    associativity right
    precedence 90
    assignment
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

infix operator &- {
    associativity left
    precedence 140
}

infix operator === {
    associativity none
    precedence 130
}

infix operator ..< {
    associativity none
    precedence 135
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

infix operator ?? {
    associativity right
    precedence 131
}

infix operator ^ {
    associativity left
    precedence 140
}

infix operator > {
    associativity none
    precedence 130
}

infix operator &= {
    associativity right
    precedence 90
    assignment
}

infix operator % {
    associativity left
    precedence 150
}

infix operator & {
    associativity left
    precedence 150
}

infix operator *= {
    associativity right
    precedence 90
    assignment
}

infix operator * {
    associativity left
    precedence 150
}

infix operator != {
    associativity none
    precedence 130
}

infix operator + {
    associativity left
    precedence 140
}

infix operator >> {
    associativity none
    precedence 160
}

infix operator - {
    associativity left
    precedence 140
}

infix operator / {
    associativity left
    precedence 150
}

infix operator <<= {
    associativity right
    precedence 90
    assignment
}

infix operator | {
    associativity left
    precedence 140
}

infix operator < {
    associativity none
    precedence 130
}

infix operator || {
    associativity left
    precedence 110
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

infix operator ~> {
    associativity left
    precedence 255
}

infix operator >= {
    associativity none
    precedence 130
}

infix operator !== {
    associativity none
    precedence 130
}

prefix operator - {
}

prefix operator -- {
}

prefix operator ++ {
}

prefix operator + {
}

prefix operator ! {
}

prefix operator ~ {
}

postfix operator -- {
}

postfix operator ++ {
}


/// Return the result of inverting `a`'s logic value.
prefix func !<T : BooleanType>(a: T) -> Bool

prefix func !(a: Bool) -> Bool


/// Returns true if the arrays do not contain the same elements.
func !=<T : Equatable>(lhs: ContiguousArray<T>, rhs: ContiguousArray<T>) -> Bool


/// Returns true if the arrays do not contain the same elements.
func !=<T : Equatable>(lhs: ArraySlice<T>, rhs: ArraySlice<T>) -> Bool


/// Returns true if the arrays do not contain the same elements.
func !=<T : Equatable>(lhs: [T], rhs: [T]) -> Bool


/// Returns true if the arrays do not contain the same elements.
func !=<T : Equatable>(lhs: _UnitTestArray<T>, rhs: _UnitTestArray<T>) -> Bool


/// Return `false` iff `t0` is identical to `t1`; i.e. if they are both
/// `nil` or they both represent the same type.
func !=(t0: Any.Type?, t1: Any.Type?) -> Bool


/// Returns `true` iff `lhs.rawValue != rhs.rawValue`.
func !=<T : RawRepresentable where T.RawValue : Equatable>(lhs: T, rhs: T) -> Bool


/// Returns `true` iff `lhs.rawValue != rhs.rawValue`.
func !=<T : Equatable where T : RawRepresentable, T.RawValue : Equatable>(lhs: T, rhs: T) -> Bool

func !=(lhs: UInt8, rhs: UInt8) -> Bool

func !=(lhs: Int8, rhs: Int8) -> Bool

func !=(lhs: UInt16, rhs: UInt16) -> Bool

func !=(lhs: Int16, rhs: Int16) -> Bool

func !=(lhs: UInt32, rhs: UInt32) -> Bool

func !=(lhs: Int32, rhs: Int32) -> Bool

func !=(lhs: UInt64, rhs: UInt64) -> Bool

func !=(lhs: Int64, rhs: Int64) -> Bool

func !=(lhs: UInt, rhs: UInt) -> Bool

func !=(lhs: Int, rhs: Int) -> Bool

func !=(lhs: Float, rhs: Float) -> Bool

func !=(lhs: Double, rhs: Double) -> Bool

func !=(lhs: Float80, rhs: Float80) -> Bool

func !=<Key : Equatable, Value : Equatable>(lhs: [Key : Value], rhs: [Key : Value]) -> Bool

func !=<T : Equatable>(lhs: T?, rhs: T?) -> Bool

func !=<T>(lhs: T?, rhs: _OptionalNilComparisonType) -> Bool

func !=<T>(lhs: _OptionalNilComparisonType, rhs: T?) -> Bool

func !=<T : Equatable>(lhs: T, rhs: T) -> Bool

func !==(lhs: AnyObject?, rhs: AnyObject?) -> Bool


/// Returns false iff `lhs` and `rhs` store the same underlying collection.
func !==<L : AnyCollectionType, R : AnyCollectionType>(lhs: L, rhs: R) -> Bool

func %(lhs: UInt, rhs: UInt) -> UInt

func %(lhs: Int64, rhs: Int64) -> Int64

func %(lhs: UInt64, rhs: UInt64) -> UInt64

func %(lhs: Int32, rhs: Int32) -> Int32

func %(lhs: UInt32, rhs: UInt32) -> UInt32

func %(lhs: Int16, rhs: Int16) -> Int16

func %(lhs: UInt16, rhs: UInt16) -> UInt16

func %(lhs: Int8, rhs: Int8) -> Int8

func %(lhs: UInt8, rhs: UInt8) -> UInt8

func %(lhs: Int, rhs: Int) -> Int

func %(lhs: Float, rhs: Float) -> Float

func %(lhs: Double, rhs: Double) -> Double

func %(lhs: Float80, rhs: Float80) -> Float80


/// Divide `lhs` and `rhs`, returning the remainder and trapping in case of
/// arithmetic overflow (except in -Ounchecked builds).
func %<T : _IntegerArithmeticType>(lhs: T, rhs: T) -> T

func %=(inout lhs: Double, rhs: Double)

func %=(inout lhs: Float, rhs: Float)


/// remainder `lhs` and `rhs` and store the result in `lhs`, trapping in
/// case of arithmetic overflow (except in -Ounchecked builds).
func %=<T : _IntegerArithmeticType>(inout lhs: T, rhs: T)

func %=(inout lhs: Float80, rhs: Float80)

func &(lhs: Int, rhs: Int) -> Int

func &(lhs: UInt, rhs: UInt) -> UInt

func &(lhs: Int64, rhs: Int64) -> Int64

func &(lhs: UInt64, rhs: UInt64) -> UInt64

func &(lhs: UInt32, rhs: UInt32) -> UInt32

func &(lhs: Int16, rhs: Int16) -> Int16

func &(lhs: UInt16, rhs: UInt16) -> UInt16

func &(lhs: Int8, rhs: Int8) -> Int8

func &(lhs: UInt8, rhs: UInt8) -> UInt8

func &<T : _RawOptionSetType>(a: T, b: T) -> T

func &(lhs: Int32, rhs: Int32) -> Int32

func &&<T : BooleanType>(lhs: T, @autoclosure rhs: () -> Bool) -> Bool


/// If `lhs` is `false`, return it.  Otherwise, evaluate `rhs` and
/// return its `boolValue`.
func &&<T : BooleanType, U : BooleanType>(lhs: T, @autoclosure rhs: () -> U) -> Bool


/// multiply `lhs` and `rhs`, silently discarding any overflow.
func &*<T : _IntegerArithmeticType>(lhs: T, rhs: T) -> T


/// add `lhs` and `rhs`, silently discarding any overflow.
func &+<T : _IntegerArithmeticType>(lhs: T, rhs: T) -> T


/// subtract `lhs` and `rhs`, silently discarding any overflow.
func &-<T : _IntegerArithmeticType>(lhs: T, rhs: T) -> T

func &=(inout lhs: UInt, rhs: UInt)

func &=(inout lhs: Int, rhs: Int)

func &=<T : BitwiseOperationsType>(inout lhs: T, rhs: T)

func &=(inout lhs: Int64, rhs: Int64)

func &=(inout lhs: UInt64, rhs: UInt64)

func &=(inout lhs: Int32, rhs: Int32)

func &=(inout lhs: UInt32, rhs: UInt32)

func &=(inout lhs: Int16, rhs: Int16)

func &=(inout lhs: UInt16, rhs: UInt16)

func &=(inout lhs: Int8, rhs: Int8)

func &=(inout lhs: UInt8, rhs: UInt8)

func *(lhs: UInt32, rhs: UInt32) -> UInt32


/// Multiply `lhs` and `rhs`, returning a result and trapping in case of
/// arithmetic overflow (except in -Ounchecked builds).
func *<T : _IntegerArithmeticType>(lhs: T, rhs: T) -> T

func *(lhs: Float80, rhs: Float80) -> Float80

func *(lhs: Double, rhs: Double) -> Double

func *(lhs: Float, rhs: Float) -> Float

func *(lhs: Int, rhs: Int) -> Int

func *(lhs: UInt, rhs: UInt) -> UInt

func *(lhs: Int64, rhs: Int64) -> Int64

func *(lhs: UInt8, rhs: UInt8) -> UInt8

func *(lhs: Int8, rhs: Int8) -> Int8

func *(lhs: UInt16, rhs: UInt16) -> UInt16

func *(lhs: Int16, rhs: Int16) -> Int16

func *(lhs: UInt64, rhs: UInt64) -> UInt64

func *(lhs: Int32, rhs: Int32) -> Int32

func *=(inout lhs: Int8, rhs: Int8)

func *=(inout lhs: UInt16, rhs: UInt16)

func *=(inout lhs: Int16, rhs: Int16)

func *=(inout lhs: UInt32, rhs: UInt32)

func *=(inout lhs: Int32, rhs: Int32)

func *=(inout lhs: UInt64, rhs: UInt64)

func *=(inout lhs: Int64, rhs: Int64)

func *=(inout lhs: UInt, rhs: UInt)

func *=(inout lhs: Int, rhs: Int)

func *=(inout lhs: Float, rhs: Float)

func *=(inout lhs: Double, rhs: Double)

func *=(inout lhs: Float80, rhs: Float80)


/// multiply `lhs` and `rhs` and store the result in `lhs`, trapping in
/// case of arithmetic overflow (except in -Ounchecked builds).
func *=<T : _IntegerArithmeticType>(inout lhs: T, rhs: T)

func *=(inout lhs: UInt8, rhs: UInt8)

func +<T : Strideable>(lhs: T, rhs: T.Stride) -> T

func +<EC1 : ExtensibleCollectionType, EC2 : ExtensibleCollectionType where EC1.Generator.Element == EC2.Generator.Element>(lhs: EC1, rhs: EC2) -> EC1

func +<T>(lhs: Int, rhs: UnsafePointer<T>) -> UnsafePointer<T>

func +<T>(lhs: UnsafePointer<T>, rhs: Int) -> UnsafePointer<T>

func +<T>(lhs: Int, rhs: UnsafeMutablePointer<T>) -> UnsafeMutablePointer<T>

func +<T>(lhs: UnsafeMutablePointer<T>, rhs: Int) -> UnsafeMutablePointer<T>

func +(lhs: String, rhs: String) -> String

func +<T : _UnsignedIntegerType>(lhs: T._DisallowMixedSignArithmetic, rhs: T) -> T

func +(lhs: UInt8, rhs: UInt8) -> UInt8

func +(lhs: Int8, rhs: Int8) -> Int8

func +(lhs: UInt16, rhs: UInt16) -> UInt16

func +(lhs: Int16, rhs: Int16) -> Int16

func +(lhs: UInt32, rhs: UInt32) -> UInt32

func +(lhs: Int32, rhs: Int32) -> Int32

func +(lhs: UInt64, rhs: UInt64) -> UInt64

func +(lhs: Int64, rhs: Int64) -> Int64

func +(lhs: UInt, rhs: UInt) -> UInt

func +(lhs: Int, rhs: Int) -> Int

prefix func +(x: Float) -> Float

func +<T : _UnsignedIntegerType>(lhs: T, rhs: T._DisallowMixedSignArithmetic) -> T

func +(lhs: Float, rhs: Float) -> Float

prefix func +(x: Double) -> Double

func +(lhs: Double, rhs: Double) -> Double

prefix func +(x: Float80) -> Float80

func +(lhs: Float80, rhs: Float80) -> Float80


/// Add `lhs` and `rhs`, returning a result and trapping in case of
/// arithmetic overflow (except in -Ounchecked builds).
func +<T : _IntegerArithmeticType>(lhs: T, rhs: T) -> T

prefix func +<T : SignedNumberType>(x: T) -> T

func +<C : ExtensibleCollectionType, S : SequenceType where S.Generator.Element == C.Generator.Element>(lhs: C, rhs: S) -> C

func +<C : ExtensibleCollectionType, S : SequenceType where S.Generator.Element == C.Generator.Element>(lhs: S, rhs: C) -> C

func +<C : ExtensibleCollectionType, S : CollectionType where S.Generator.Element == C.Generator.Element>(lhs: C, rhs: S) -> C

func +<T : Strideable>(lhs: T.Stride, rhs: T) -> T

postfix func ++(inout x: Int32) -> Int32

prefix func ++(inout x: Int32) -> Int32

prefix func ++(inout x: Int16) -> Int16

prefix func ++(inout rhs: Double) -> Double

postfix func ++(inout lhs: Double) -> Double

prefix func ++(inout rhs: Float80) -> Float80

postfix func ++(inout lhs: Float80) -> Float80

postfix func ++(inout x: UInt16) -> UInt16


/// Replace `i` with its `successor()` and return the updated value of
/// `i`.
prefix func ++<T : _Incrementable>(inout i: T) -> T


/// Replace `i` with its `successor()` and return the original
/// value of `i`.
postfix func ++<T : _Incrementable>(inout i: T) -> T

prefix func ++(inout x: UInt64) -> UInt64

postfix func ++(inout x: UInt64) -> UInt64

prefix func ++(inout x: Int64) -> Int64

postfix func ++(inout x: UInt32) -> UInt32

prefix func ++(inout x: UInt32) -> UInt32

postfix func ++(inout x: Int16) -> Int16

postfix func ++(inout x: Int64) -> Int64

prefix func ++(inout x: UInt) -> UInt

postfix func ++(inout x: UInt) -> UInt

prefix func ++(inout x: Int) -> Int

postfix func ++(inout x: Int) -> Int

prefix func ++(inout rhs: Float) -> Float

postfix func ++(inout lhs: Float) -> Float

prefix func ++(inout x: UInt8) -> UInt8

postfix func ++(inout x: UInt8) -> UInt8

prefix func ++(inout x: Int8) -> Int8

prefix func ++(inout x: UInt16) -> UInt16

postfix func ++(inout x: Int8) -> Int8

func +=(inout lhs: UInt16, rhs: UInt16)

func +=(inout lhs: Int8, rhs: Int8)

func +=(inout lhs: UInt8, rhs: UInt8)


/// Append the elements of `rhs` to `lhs`.
func +=<T, C : CollectionType where C.Generator.Element == T>(inout lhs: _ContiguousArrayBuffer<T>, rhs: C)


/// Extend `lhs` with the elements of `rhs`.
func +=<T, C : CollectionType where C.Generator.Element == T>(inout lhs: _UnitTestArray<T>, rhs: C)


/// Extend `lhs` with the elements of `rhs`.
func +=<T, S : SequenceType where S.Generator.Element == T>(inout lhs: _UnitTestArray<T>, rhs: S)


/// Extend `lhs` with the elements of `rhs`.
func +=<T, C : CollectionType where C.Generator.Element == T>(inout lhs: [T], rhs: C)


/// Extend `lhs` with the elements of `rhs`.
func +=<T, S : SequenceType where S.Generator.Element == T>(inout lhs: [T], rhs: S)


/// Extend `lhs` with the elements of `rhs`.
func +=<T, C : CollectionType where C.Generator.Element == T>(inout lhs: ArraySlice<T>, rhs: C)

func +=(inout lhs: Int16, rhs: Int16)

func +=(inout lhs: UInt32, rhs: UInt32)

func +=(inout lhs: Int32, rhs: Int32)

func +=(inout lhs: UInt64, rhs: UInt64)


/// Extend `lhs` with the elements of `rhs`.
func +=<T, S : SequenceType where S.Generator.Element == T>(inout lhs: ArraySlice<T>, rhs: S)


/// Extend `lhs` with the elements of `rhs`.
func +=<T, C : CollectionType where C.Generator.Element == T>(inout lhs: ContiguousArray<T>, rhs: C)


/// Extend `lhs` with the elements of `rhs`.
func +=<T, S : SequenceType where S.Generator.Element == T>(inout lhs: ContiguousArray<T>, rhs: S)

func +=(inout lhs: Int64, rhs: Int64)

func +=(inout lhs: UInt, rhs: UInt)

func +=(inout lhs: Int, rhs: Int)

func +=(inout lhs: Float, rhs: Float)

func +=(inout lhs: Double, rhs: Double)

func +=(inout lhs: Float80, rhs: Float80)


/// add `lhs` and `rhs` and store the result in `lhs`, trapping in
/// case of arithmetic overflow (except in -Ounchecked builds).
func +=<T : _IntegerArithmeticType>(inout lhs: T, rhs: T)

func +=<T : Strideable>(inout lhs: T, rhs: T.Stride)

func +=<T>(inout lhs: UnsafePointer<T>, rhs: Int)

func +=(inout lhs: String, rhs: String)


/// Append the elements of `rhs` to `lhs`.
func +=<T, C : CollectionType where C.Generator.Element == T>(inout lhs: _UnitTestArrayBuffer<T>, rhs: C)


/// Append `rhs` to `lhs`.
func +=<T>(inout lhs: _UnitTestArrayBuffer<T>, rhs: T)

func +=<T>(inout lhs: UnsafeMutablePointer<T>, rhs: Int)

func +=<T : _UnsignedIntegerType>(inout lhs: T, rhs: T._DisallowMixedSignArithmetic)


/// Subtract `lhs` and `rhs`, returning a result and trapping in case of
/// arithmetic overflow (except in -Ounchecked builds).
func -<T : _IntegerArithmeticType>(lhs: T, rhs: T) -> T

prefix func -<T : SignedNumberType>(x: T) -> T

func -(lhs: UInt8, rhs: UInt8) -> UInt8

func -(lhs: Int8, rhs: Int8) -> Int8

func -(lhs: UInt16, rhs: UInt16) -> UInt16

func -(lhs: Int16, rhs: Int16) -> Int16

func -(lhs: UInt32, rhs: UInt32) -> UInt32

func -(lhs: Int32, rhs: Int32) -> Int32

func -(lhs: UInt64, rhs: UInt64) -> UInt64

func -(lhs: Int64, rhs: Int64) -> Int64

func -(lhs: UInt, rhs: UInt) -> UInt

func -(lhs: Int, rhs: Int) -> Int

prefix func -(x: Float) -> Float

func -(lhs: Float, rhs: Float) -> Float

prefix func -(x: Double) -> Double

func -(lhs: Double, rhs: Double) -> Double

prefix func -(x: Float80) -> Float80

func -(lhs: Float80, rhs: Float80) -> Float80

func -<T : Strideable>(lhs: T, rhs: T.Stride) -> T

func -<T>(lhs: UnsafePointer<T>, rhs: UnsafePointer<T>) -> Int

func -<T>(lhs: UnsafePointer<T>, rhs: Int) -> UnsafePointer<T>

func -<T>(lhs: UnsafeMutablePointer<T>, rhs: UnsafeMutablePointer<T>) -> Int

func -<T>(lhs: UnsafeMutablePointer<T>, rhs: Int) -> UnsafeMutablePointer<T>

func -<T : _UnsignedIntegerType>(lhs: T, rhs: T) -> T._DisallowMixedSignArithmetic

func -<T : _UnsignedIntegerType>(lhs: T, rhs: T._DisallowMixedSignArithmetic) -> T

func -<T : Strideable>(lhs: T, rhs: T) -> T.Stride

prefix func --(inout x: Int) -> Int

prefix func --(inout rhs: Double) -> Double

postfix func --(inout lhs: Float) -> Float

postfix func --(inout x: Int) -> Int

prefix func --(inout rhs: Float) -> Float

prefix func --(inout x: UInt32) -> UInt32

postfix func --(inout x: UInt32) -> UInt32

prefix func --(inout x: Int32) -> Int32

postfix func --(inout x: Int32) -> Int32

prefix func --(inout x: UInt64) -> UInt64

postfix func --(inout x: UInt64) -> UInt64

prefix func --(inout x: Int64) -> Int64

postfix func --(inout x: Int64) -> Int64

prefix func --(inout x: UInt) -> UInt


/// Replace `i` with its `predecessor()` and return the original
/// value of `i`.
postfix func --<T : BidirectionalIndexType>(inout i: T) -> T


/// Replace `i` with its `predecessor()` and return the updated value
/// of `i`.
prefix func --<T : BidirectionalIndexType>(inout i: T) -> T

postfix func --(inout lhs: Float80) -> Float80

prefix func --(inout rhs: Float80) -> Float80

postfix func --(inout lhs: Double) -> Double

postfix func --(inout x: UInt) -> UInt

postfix func --(inout x: Int16) -> Int16

prefix func --(inout x: Int16) -> Int16

postfix func --(inout x: UInt16) -> UInt16

prefix func --(inout x: UInt16) -> UInt16

postfix func --(inout x: Int8) -> Int8

prefix func --(inout x: Int8) -> Int8

postfix func --(inout x: UInt8) -> UInt8

prefix func --(inout x: UInt8) -> UInt8

func -=(inout lhs: UInt8, rhs: UInt8)

func -=<T : _UnsignedIntegerType>(inout lhs: T, rhs: T._DisallowMixedSignArithmetic)

func -=<T>(inout lhs: UnsafePointer<T>, rhs: Int)

func -=<T>(inout lhs: UnsafeMutablePointer<T>, rhs: Int)

func -=<T : Strideable>(inout lhs: T, rhs: T.Stride)


/// subtract `lhs` and `rhs` and store the result in `lhs`, trapping in
/// case of arithmetic overflow (except in -Ounchecked builds).
func -=<T : _IntegerArithmeticType>(inout lhs: T, rhs: T)

func -=(inout lhs: Float80, rhs: Float80)

func -=(inout lhs: Double, rhs: Double)

func -=(inout lhs: Float, rhs: Float)

func -=(inout lhs: Int, rhs: Int)

func -=(inout lhs: UInt, rhs: UInt)

func -=(inout lhs: Int64, rhs: Int64)

func -=(inout lhs: UInt64, rhs: UInt64)

func -=(inout lhs: Int32, rhs: Int32)

func -=(inout lhs: UInt32, rhs: UInt32)

func -=(inout lhs: Int16, rhs: Int16)

func -=(inout lhs: Int8, rhs: Int8)

func -=(inout lhs: UInt16, rhs: UInt16)


/// Returns a closed interval from `start` through `end`.
func ...<T : Comparable>(start: T, end: T) -> ClosedInterval<T>


/// Forms a closed range that contains both `minimum` and `maximum`.
func ...<Pos : ForwardIndexType>(minimum: Pos, maximum: Pos) -> Range<Pos>


/// Forms a closed range that contains both `start` and `end`.
/// - Requires: `start <= end`.
func ...<Pos : ForwardIndexType where Pos : Comparable>(start: Pos, end: Pos) -> Range<Pos>


/// Returns a half-open interval from `start` to `end`.
func ..<<T : Comparable>(start: T, end: T) -> HalfOpenInterval<T>


/// Forms a half-open range that contains `minimum`, but not
/// `maximum`.
func ..<<Pos : ForwardIndexType>(minimum: Pos, maximum: Pos) -> Range<Pos>


/// Forms a half-open range that contains `start`, but not `end`.
///
/// - Requires: `start <= end`.
func ..<<Pos : ForwardIndexType where Pos : Comparable>(start: Pos, end: Pos) -> Range<Pos>


/// Divide `lhs` and `rhs`, returning a result and trapping in case of
/// arithmetic overflow (except in -Ounchecked builds).
func /<T : _IntegerArithmeticType>(lhs: T, rhs: T) -> T

func /(lhs: Float80, rhs: Float80) -> Float80

func /(lhs: Double, rhs: Double) -> Double

func /(lhs: Float, rhs: Float) -> Float

func /(lhs: Int, rhs: Int) -> Int

func /(lhs: UInt, rhs: UInt) -> UInt

func /(lhs: Int64, rhs: Int64) -> Int64

func /(lhs: UInt64, rhs: UInt64) -> UInt64

func /(lhs: Int32, rhs: Int32) -> Int32

func /(lhs: UInt32, rhs: UInt32) -> UInt32

func /(lhs: Int16, rhs: Int16) -> Int16

func /(lhs: UInt16, rhs: UInt16) -> UInt16

func /(lhs: Int8, rhs: Int8) -> Int8

func /(lhs: UInt8, rhs: UInt8) -> UInt8

func /=(inout lhs: Float, rhs: Float)

func /=(inout lhs: Double, rhs: Double)

func /=(inout lhs: Float80, rhs: Float80)


/// divide `lhs` and `rhs` and store the result in `lhs`, trapping in
/// case of arithmetic overflow (except in -Ounchecked builds).
func /=<T : _IntegerArithmeticType>(inout lhs: T, rhs: T)

func <(lhs: UInt32, rhs: UInt32) -> Bool

func <(lhs: Float, rhs: Float) -> Bool

func <(lhs: Bit, rhs: Bit) -> Bool


/// Lexicographic comparison of version components.
func <(left: _SwiftNSOperatingSystemVersion, right: _SwiftNSOperatingSystemVersion) -> Bool

func <<T>(lhs: UnsafePointer<T>, rhs: UnsafePointer<T>) -> Bool

func <<T>(lhs: UnsafeMutablePointer<T>, rhs: UnsafeMutablePointer<T>) -> Bool

func <(lhs: UnicodeScalar, rhs: UnicodeScalar) -> Bool

func <(lhs: String.UTF16View.Index, rhs: String.UTF16View.Index) -> Bool

func <(lhs: String.UnicodeScalarView.Index, rhs: String.UnicodeScalarView.Index) -> Bool

func <(lhs: UInt64, rhs: UInt64) -> Bool

func <(lhs: Index, rhs: Index) -> Bool

func <(lhs: String, rhs: String) -> Bool


/// Compare two `Strideable`s.
func <<T : _Strideable>(x: T, y: T) -> Bool

func <(lhs: ObjectIdentifier, rhs: ObjectIdentifier) -> Bool

func <<T : Comparable>(lhs: T?, rhs: T?) -> Bool

func <<Key : Hashable, Value>(lhs: DictionaryIndex<Key, Value>, rhs: DictionaryIndex<Key, Value>) -> Bool

func <<T : Hashable>(lhs: SetIndex<T>, rhs: SetIndex<T>) -> Bool

func <(lhs: Float80, rhs: Float80) -> Bool

func <(lhs: Double, rhs: Double) -> Bool

func <(lhs: Int16, rhs: Int16) -> Bool

func <(lhs: Int, rhs: Int) -> Bool

func <(lhs: UInt, rhs: UInt) -> Bool

func <(lhs: UInt16, rhs: UInt16) -> Bool

func <(lhs: Int8, rhs: Int8) -> Bool

func <(lhs: Character, rhs: Character) -> Bool

func <(lhs: UInt8, rhs: UInt8) -> Bool

func <(lhs: Int32, rhs: Int32) -> Bool

func <(lhs: Int64, rhs: Int64) -> Bool

func <<(lhs: Int16, rhs: Int16) -> Int16

func <<(lhs: UInt16, rhs: UInt16) -> UInt16

func <<(lhs: Int8, rhs: Int8) -> Int8

func <<(lhs: UInt8, rhs: UInt8) -> UInt8

func <<(lhs: UInt32, rhs: UInt32) -> UInt32

func <<(lhs: Int32, rhs: Int32) -> Int32

func <<(lhs: UInt64, rhs: UInt64) -> UInt64

func <<(lhs: UInt, rhs: UInt) -> UInt

func <<(lhs: Int, rhs: Int) -> Int

func <<(lhs: Int64, rhs: Int64) -> Int64

func <<=(inout lhs: Int8, rhs: Int8)

func <<=(inout lhs: UInt, rhs: UInt)

func <<=(inout lhs: Int, rhs: Int)

func <<=(inout lhs: UInt8, rhs: UInt8)

func <<=(inout lhs: UInt16, rhs: UInt16)

func <<=(inout lhs: Int64, rhs: Int64)

func <<=(inout lhs: Int16, rhs: Int16)

func <<=(inout lhs: UInt32, rhs: UInt32)

func <<=(inout lhs: Int32, rhs: Int32)

func <<=(inout lhs: UInt64, rhs: UInt64)

func <=(lhs: Float, rhs: Float) -> Bool

func <=(lhs: UInt32, rhs: UInt32) -> Bool

func <=<T : Comparable>(lhs: T, rhs: T) -> Bool

func <=<T : Comparable>(lhs: T?, rhs: T?) -> Bool

func <=(lhs: Int, rhs: Int) -> Bool

func <=(lhs: UInt, rhs: UInt) -> Bool

func <=(lhs: Int64, rhs: Int64) -> Bool

func <=(lhs: UInt64, rhs: UInt64) -> Bool

func <=(lhs: Int32, rhs: Int32) -> Bool

func <=(lhs: Int16, rhs: Int16) -> Bool

func <=(lhs: UInt16, rhs: UInt16) -> Bool

func <=(lhs: Int8, rhs: Int8) -> Bool

func <=(lhs: UInt8, rhs: UInt8) -> Bool

func <=(lhs: Double, rhs: Double) -> Bool

func <=(lhs: Float80, rhs: Float80) -> Bool

func ==(lhs: Int8, rhs: Int8) -> Bool

func ==<I>(lhs: _ConcatenateForwardIndex<I>, rhs: _ConcatenateForwardIndex<I>) -> Bool

func ==<T : _RawOptionSetType>(a: T, b: T) -> Bool

func ==(lhs: FloatingPointClassification, rhs: FloatingPointClassification) -> Bool

func ==<I>(lhs: _ConcatenateBidirectionalIndex<I>, rhs: _ConcatenateBidirectionalIndex<I>) -> Bool


/// Returns `true` iff `lhs.rawValue == rhs.rawValue`.
func ==<T : RawRepresentable where T.RawValue : Equatable>(lhs: T, rhs: T) -> Bool

func ==(lhs: Character, rhs: Character) -> Bool

func ==(lhs: COpaquePointer, rhs: COpaquePointer) -> Bool

func ==<T : Hashable>(lhs: Set<T>, rhs: Set<T>) -> Bool

func ==<Key : Equatable, Value : Equatable>(lhs: [Key : Value], rhs: [Key : Value]) -> Bool


/// Return `true` iff `t0` is identical to `t1`; i.e. if they are both
/// `nil` or they both represent the same type.
func ==(t0: Any.Type?, t1: Any.Type?) -> Bool

func ==<T>(lhs: AutoreleasingUnsafeMutablePointer<T>, rhs: AutoreleasingUnsafeMutablePointer<T>) -> Bool

func ==(lhs: Float80, rhs: Float80) -> Bool

func ==(lhs: Bool, rhs: Bool) -> Bool


/// Returns true if these arrays contain the same elements.
func ==<T : Equatable>(lhs: _UnitTestArray<T>, rhs: _UnitTestArray<T>) -> Bool


/// Returns true if these arrays contain the same elements.
func ==<T : Equatable>(lhs: [T], rhs: [T]) -> Bool


/// Returns true if these arrays contain the same elements.
func ==<T : Equatable>(lhs: ArraySlice<T>, rhs: ArraySlice<T>) -> Bool


/// Returns true if these arrays contain the same elements.
func ==<T : Equatable>(lhs: ContiguousArray<T>, rhs: ContiguousArray<T>) -> Bool

func ==(lhs: Double, rhs: Double) -> Bool

func ==(lhs: Float, rhs: Float) -> Bool

func ==(lhs: Int, rhs: Int) -> Bool

func ==(lhs: UInt, rhs: UInt) -> Bool

func ==(lhs: UInt8, rhs: UInt8) -> Bool

func ==(lhs: Int64, rhs: Int64) -> Bool

func ==(lhs: UInt64, rhs: UInt64) -> Bool

func ==<T : Hashable>(lhs: SetIndex<T>, rhs: SetIndex<T>) -> Bool

func ==<Key : Hashable, Value>(lhs: DictionaryIndex<Key, Value>, rhs: DictionaryIndex<Key, Value>) -> Bool


/// Return true iff `lhs` and `rhs` wrap equal underlying
/// `AnyRandomAccessIndex`s.
///
/// - Requires: The types of indices wrapped by `lhs` and `rhs` are
///   identical.
func ==(lhs: AnyRandomAccessIndex, rhs: AnyRandomAccessIndex) -> Bool


/// Two `HalfOpenInterval`s are equal if their `start` and `end` are
/// equal.
func ==<T : Comparable>(lhs: HalfOpenInterval<T>, rhs: HalfOpenInterval<T>) -> Bool


/// Two `ClosedInterval`s are equal if their `start` and `end` are
/// equal.
func ==<T : Comparable>(lhs: ClosedInterval<T>, rhs: ClosedInterval<T>) -> Bool

func ==<Value, Element>(lhs: ManagedBufferPointer<Value, Element>, rhs: ManagedBufferPointer<Value, Element>) -> Bool

func ==<T : Equatable>(lhs: T?, rhs: T?) -> Bool

func ==<T>(lhs: T?, rhs: _OptionalNilComparisonType) -> Bool

func ==<T>(lhs: _OptionalNilComparisonType, rhs: T?) -> Bool

func ==<T>(lhs: Range<T>, rhs: Range<T>) -> Bool

func ==(x: ObjectIdentifier, y: ObjectIdentifier) -> Bool

func ==<I>(lhs: ReverseBidirectionalIndex<I>, rhs: ReverseBidirectionalIndex<I>) -> Bool

func ==<I>(lhs: ReverseRandomAccessIndex<I>, rhs: ReverseRandomAccessIndex<I>) -> Bool

func ==(lhs: Int32, rhs: Int32) -> Bool

func ==(lhs: UInt32, rhs: UInt32) -> Bool

func ==(lhs: Int16, rhs: Int16) -> Bool

func ==<T : _Strideable>(x: T, y: T) -> Bool

func ==(lhs: String, rhs: String) -> Bool

func ==(lhs: Index, rhs: Index) -> Bool

func ==(lhs: UInt16, rhs: UInt16) -> Bool

func ==(lhs: String.UnicodeScalarView.Index, rhs: String.UnicodeScalarView.Index) -> Bool

func ==(lhs: String.UTF16View.Index, rhs: String.UTF16View.Index) -> Bool

func ==<Base : CollectionType>(lhs: FilterCollectionViewIndex<Base>, rhs: FilterCollectionViewIndex<Base>) -> Bool

func ==(lhs: String.UTF8View.Index, rhs: String.UTF8View.Index) -> Bool

func ==(lhs: UnicodeScalar, rhs: UnicodeScalar) -> Bool

func ==<T>(lhs: UnsafeMutablePointer<T>, rhs: UnsafeMutablePointer<T>) -> Bool

func ==<T>(lhs: UnsafePointer<T>, rhs: UnsafePointer<T>) -> Bool

func ==(left: _SwiftNSOperatingSystemVersion, right: _SwiftNSOperatingSystemVersion) -> Bool

func ==(lhs: Bit, rhs: Bit) -> Bool


/// Return true iff `lhs` and `rhs` wrap equal underlying
/// `AnyForwardIndex`s.
///
/// - Requires: The types of indices wrapped by `lhs` and `rhs` are
///   identical.
func ==(lhs: AnyForwardIndex, rhs: AnyForwardIndex) -> Bool


/// Return true iff `lhs` and `rhs` wrap equal underlying
/// `AnyBidirectionalIndex`s.
///
/// - Requires: The types of indices wrapped by `lhs` and `rhs` are
///   identical.
func ==(lhs: AnyBidirectionalIndex, rhs: AnyBidirectionalIndex) -> Bool


/// Return true iff `lhs` and `rhs` store the same underlying collection.
func ===<L : AnyCollectionType, R : AnyCollectionType>(lhs: L, rhs: R) -> Bool

func ===(lhs: AnyObject?, rhs: AnyObject?) -> Bool

func >(lhs: Int, rhs: Int) -> Bool

func >(lhs: UInt, rhs: UInt) -> Bool

func >(lhs: Int64, rhs: Int64) -> Bool

func >(lhs: Float, rhs: Float) -> Bool

func >(lhs: Double, rhs: Double) -> Bool

func >(lhs: Float80, rhs: Float80) -> Bool

func >(lhs: Int32, rhs: Int32) -> Bool

func >(lhs: UInt64, rhs: UInt64) -> Bool

func >(lhs: UInt8, rhs: UInt8) -> Bool

func ><T : Comparable>(lhs: T?, rhs: T?) -> Bool

func ><T : Comparable>(lhs: T, rhs: T) -> Bool

func >(lhs: Int8, rhs: Int8) -> Bool

func >(lhs: UInt16, rhs: UInt16) -> Bool

func >(lhs: Int16, rhs: Int16) -> Bool

func >(lhs: UInt32, rhs: UInt32) -> Bool

func >=(lhs: UInt, rhs: UInt) -> Bool

func >=(lhs: UInt8, rhs: UInt8) -> Bool

func >=(lhs: Int8, rhs: Int8) -> Bool

func >=(lhs: UInt16, rhs: UInt16) -> Bool

func >=(lhs: Int16, rhs: Int16) -> Bool

func >=(lhs: UInt32, rhs: UInt32) -> Bool

func >=(lhs: Int32, rhs: Int32) -> Bool

func >=(lhs: UInt64, rhs: UInt64) -> Bool

func >=(lhs: Int64, rhs: Int64) -> Bool

func >=(lhs: Int, rhs: Int) -> Bool

func >=(lhs: Float, rhs: Float) -> Bool

func >=(lhs: Double, rhs: Double) -> Bool

func >=(lhs: Float80, rhs: Float80) -> Bool

func >=<T : Comparable>(lhs: T?, rhs: T?) -> Bool

func >=<T : Comparable>(lhs: T, rhs: T) -> Bool

func >=(left: _SwiftNSOperatingSystemVersion, right: _SwiftNSOperatingSystemVersion) -> Bool

func >>(lhs: UInt16, rhs: UInt16) -> UInt16

func >>(lhs: Int, rhs: Int) -> Int

func >>(lhs: UInt, rhs: UInt) -> UInt

func >>(lhs: Int64, rhs: Int64) -> Int64

func >>(lhs: UInt64, rhs: UInt64) -> UInt64

func >>(lhs: Int32, rhs: Int32) -> Int32

func >>(lhs: UInt32, rhs: UInt32) -> UInt32

func >>(lhs: Int16, rhs: Int16) -> Int16

func >>(lhs: UInt8, rhs: UInt8) -> UInt8

func >>(lhs: Int8, rhs: Int8) -> Int8

func >>=(inout lhs: UInt16, rhs: UInt16)

func >>=(inout lhs: Int16, rhs: Int16)

func >>=(inout lhs: UInt32, rhs: UInt32)

func >>=(inout lhs: Int32, rhs: Int32)

func >>=(inout lhs: UInt64, rhs: UInt64)

func >>=(inout lhs: Int64, rhs: Int64)

func >>=(inout lhs: UInt, rhs: UInt)

func >>=(inout lhs: Int, rhs: Int)

func >>=(inout lhs: Int8, rhs: Int8)

func >>=(inout lhs: UInt8, rhs: UInt8)

func ??<T>(optional: T?, @autoclosure defaultValue: () -> T?) -> T?

func ??<T>(optional: T?, @autoclosure defaultValue: () -> T) -> T


/// A type that supports an "absolute value" function.
protocol AbsoluteValuable : SignedNumberType {

    /// Returns the absolute value of `x`.
    static func abs(x: Self) -> Self
}


/// The protocol to which all types implicitly conform.
typealias Any = protocol<>


/// A type-erased wrapper over any collection with indices that
/// support bidirectional traversal.
///
/// Forwards operations to an arbitrary underlying collection having the
/// same `Element` type, hiding the specifics of the underlying
/// `CollectionType`.
///
/// - SeeAlso: `AnyRandomAccessType`, `AnyForwardType`
struct AnyBidirectionalCollection<Element> : AnyCollectionType, CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType {

    /// Create an `AnyBidirectionalCollection` that stores `base` as its
    /// underlying collection.
    ///
    /// - Complexity: O(1).
    init<C : CollectionType where C.Index : BidirectionalIndexType, C.Generator.Element == Element>(_ base: C)

    /// Create an `AnyBidirectionalCollection` having the same underlying
    /// collection as `other`.
    ///
    /// - Postcondition: The result is `===` to `other`.
    ///
    /// - Complexity: O(1).
    init(_ other: AnyBidirectionalCollection<Element>)

    /// Create an `AnyBidirectionalCollection` that stores `base` as its
    /// underlying collection.
    ///
    /// - Complexity: O(1).
    init<C : CollectionType where C.Index : RandomAccessIndexType, C.Generator.Element == Element>(_ base: C)

    /// Create an `AnyBidirectionalCollection` having the same underlying
    /// collection as `other`.
    ///
    /// - Postcondition: The result is `===` to `other`.
    ///
    /// - Complexity: O(1).
    init(_ other: AnyRandomAccessCollection<Element>)

    /// If the indices of the underlying collection stored by `other`
    /// satisfy `BidirectionalIndexType`, create an
    /// `AnyBidirectionalCollection` having the same underlying
    /// collection as `other`.  Otherwise, the result is `nil`.
    ///
    /// - Complexity: O(1).
    init?(_ other: AnyForwardCollection<Element>)

    /// Returns a *generator* over the elements of this *collection*.
    ///
    /// - Complexity: O(1).
    func generate() -> AnyGenerator<Element>

    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    var startIndex: AnyBidirectionalIndex { get }

    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    var endIndex: AnyBidirectionalIndex { get }
    subscript (position: AnyBidirectionalIndex) -> Element { get }

    /// Return the number of elements.
    ///
    /// - Complexity: O(N).
    var count: IntMax { get }
}

extension AnyBidirectionalCollection {
    func underestimateCount() -> Int
}


/// A wrapper over an underlying `BidirectionalIndexType` that hides
/// the specific underlying type.
///
/// - SeeAlso: `AnyBidirectionalCollection`
struct AnyBidirectionalIndex : ForwardIndexType, _Incrementable, Equatable, BidirectionalIndexType {
    typealias Distance = IntMax

    /// Wrap and forward operations to `base`.
    init<BaseIndex : BidirectionalIndexType>(_ base: BaseIndex)

    /// Return the next consecutive value in a discrete sequence of
    /// `AnyBidirectionalIndex` values.
    ///
    /// - Requires: `self` has a well-defined successor.
    func successor() -> AnyBidirectionalIndex

    /// Return the previous consecutive value in a discrete sequence of
    /// `AnyBidirectionalIndex` values.
    ///
    /// - Requires: `self` has a well-defined predecessor.
    func predecessor() -> AnyBidirectionalIndex
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
typealias AnyClass = AnyObject.Type


/// A protocol for `AnyForwardCollection<T>`,
/// `AnyBidirectionalCollection<T>`, and
/// `AnyRandomAccessCollection<T>`.
///
/// This protocol can be considered an implementation detail of the
/// `===` and `!==` implementations for these types.
protocol AnyCollectionType : CollectionType {
}


/// A type-erased wrapper over any collection with indices that
/// support forward traversal.
///
/// Forwards operations to an arbitrary underlying collection having the
/// same `Element` type, hiding the specifics of the underlying
/// `CollectionType`.
///
/// - SeeAlso: `AnyBidirectionalType`, `AnyRandomAccessType`
struct AnyForwardCollection<Element> : AnyCollectionType, CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType {

    /// Create an `AnyForwardCollection` that stores `base` as its
    /// underlying collection.
    ///
    /// - Complexity: O(1).
    init<C : CollectionType where C.Index : ForwardIndexType, C.Generator.Element == Element>(_ base: C)

    /// Create an `AnyForwardCollection` having the same underlying
    /// collection as `other`.
    ///
    /// - Postcondition: The result is `===` to `other`.
    ///
    /// - Complexity: O(1).
    init(_ other: AnyForwardCollection<Element>)

    /// Create an `AnyForwardCollection` that stores `base` as its
    /// underlying collection.
    ///
    /// - Complexity: O(1).
    init<C : CollectionType where C.Index : BidirectionalIndexType, C.Generator.Element == Element>(_ base: C)

    /// Create an `AnyForwardCollection` having the same underlying
    /// collection as `other`.
    ///
    /// - Postcondition: The result is `===` to `other`.
    ///
    /// - Complexity: O(1).
    init(_ other: AnyBidirectionalCollection<Element>)

    /// Create an `AnyForwardCollection` that stores `base` as its
    /// underlying collection.
    ///
    /// - Complexity: O(1).
    init<C : CollectionType where C.Index : RandomAccessIndexType, C.Generator.Element == Element>(_ base: C)

    /// Create an `AnyForwardCollection` having the same underlying
    /// collection as `other`.
    ///
    /// - Postcondition: The result is `===` to `other`.
    ///
    /// - Complexity: O(1).
    init(_ other: AnyRandomAccessCollection<Element>)

    /// Returns a *generator* over the elements of this *collection*.
    ///
    /// - Complexity: O(1).
    func generate() -> AnyGenerator<Element>

    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    var startIndex: AnyForwardIndex { get }

    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    var endIndex: AnyForwardIndex { get }
    subscript (position: AnyForwardIndex) -> Element { get }

    /// Return the number of elements.
    ///
    /// - Complexity: O(N).
    var count: IntMax { get }
}

extension AnyForwardCollection {
    func underestimateCount() -> Int
}


/// A wrapper over an underlying `ForwardIndexType` that hides
/// the specific underlying type.
///
/// - SeeAlso: `AnyForwardCollection`
struct AnyForwardIndex : ForwardIndexType, _Incrementable, Equatable {
    typealias Distance = IntMax

    /// Wrap and forward operations to `base`.
    init<BaseIndex : ForwardIndexType>(_ base: BaseIndex)

    /// Return the next consecutive value in a discrete sequence of
    /// `AnyForwardIndex` values.
    ///
    /// - Requires: `self` has a well-defined successor.
    func successor() -> AnyForwardIndex
}


/// An abstract `GeneratorType` base class over `T` elements.
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
///   - `func anyGenerator<T>(body: ()->T?) -> AnyGenerator<T>`
class AnyGenerator<T> : GeneratorType {

    /// Initialize the instance.  May only be called from a subclass
    /// initializer.
    init()

    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Note: Subclasses must override this method.
    func next() -> T?
}

extension AnyGenerator : SequenceType {

    /// Returns `self`.
    func generate() -> AnyGenerator<T>
}

@objc protocol AnyObject {
}


/// A type-erased wrapper over any collection with indices that
/// support random access traversal.
///
/// Forwards operations to an arbitrary underlying collection having the
/// same `Element` type, hiding the specifics of the underlying
/// `CollectionType`.
///
/// - SeeAlso: `AnyForwardType`, `AnyBidirectionalType`
struct AnyRandomAccessCollection<Element> : AnyCollectionType, CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType {

    /// Create an `AnyRandomAccessCollection` that stores `base` as its
    /// underlying collection.
    ///
    /// - Complexity: O(1).
    init<C : CollectionType where C.Index : RandomAccessIndexType, C.Generator.Element == Element>(_ base: C)

    /// Create an `AnyRandomAccessCollection` having the same underlying
    /// collection as `other`.
    ///
    /// - Postcondition: The result is `===` to `other`.
    ///
    /// - Complexity: O(1).
    init(_ other: AnyRandomAccessCollection<Element>)

    /// If the indices of the underlying collection stored by `other`
    /// satisfy `RandomAccessIndexType`, create an
    /// `AnyRandomAccessCollection` having the same underlying
    /// collection as `other`.  Otherwise, the result is `nil`.
    ///
    /// - Complexity: O(1).
    init?(_ other: AnyForwardCollection<Element>)

    /// If the indices of the underlying collection stored by `other`
    /// satisfy `RandomAccessIndexType`, create an
    /// `AnyRandomAccessCollection` having the same underlying
    /// collection as `other`.  Otherwise, the result is `nil`.
    ///
    /// - Complexity: O(1).
    init?(_ other: AnyBidirectionalCollection<Element>)

    /// Returns a *generator* over the elements of this *collection*.
    ///
    /// - Complexity: O(1).
    func generate() -> AnyGenerator<Element>

    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    var startIndex: AnyRandomAccessIndex { get }

    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    var endIndex: AnyRandomAccessIndex { get }
    subscript (position: AnyRandomAccessIndex) -> Element { get }

    /// Return the number of elements.
    ///
    /// - Complexity: O(1).
    var count: IntMax { get }
}

extension AnyRandomAccessCollection {
    func underestimateCount() -> Int
}


/// A wrapper over an underlying `RandomAccessIndexType` that hides
/// the specific underlying type.
///
/// - SeeAlso: `AnyRandomAccessCollection`
struct AnyRandomAccessIndex : ForwardIndexType, _Incrementable, Equatable, RandomAccessIndexType, BidirectionalIndexType, Strideable, Comparable, _Strideable {
    typealias Distance = IntMax

    /// Wrap and forward operations to `base`.
    init<BaseIndex : RandomAccessIndexType>(_ base: BaseIndex)

    /// Return the next consecutive value in a discrete sequence of
    /// `AnyRandomAccessIndex` values.
    ///
    /// - Requires: `self` has a well-defined successor.
    func successor() -> AnyRandomAccessIndex

    /// Return the previous consecutive value in a discrete sequence of
    /// `AnyRandomAccessIndex` values.
    ///
    /// - Requires: `self` has a well-defined predecessor.
    func predecessor() -> AnyRandomAccessIndex

    /// Return the minimum number of applications of `successor` or
    /// `predecessor` required to reach `other` from `self`.
    ///
    /// - Requires: `self` and `other` wrap instances of the same type.
    func distanceTo(other: AnyRandomAccessIndex) -> Distance

    /// Return `self` offset by `n` steps.
    ///
    /// - Returns: If `n > 0`, the result of applying `successor` to
    ///   `self` `n` times.  If `n < 0`, the result of applying
    ///   `predecessor` to `self` `n` times. Otherwise, `self`.
    func advancedBy(amount: Distance) -> AnyRandomAccessIndex
}


/// A type-erased sequence.
///
/// Forwards operations to an arbitrary underlying sequence having the
/// same `Element` type, hiding the specifics of the underlying
/// `SequenceType`.
///
/// - SeeAlso: `AnyGenerator<T>`.
struct AnySequence<T> : SequenceType {
    typealias Element = T

    /// Wrap and forward operations to to `base`.
    init<S : SequenceType where S.Generator.Element == T>(_ base: S)

    /// Create a sequence whose `generate()` method forwards to
    /// `makeUnderlyingGenerator`.
    init<G : GeneratorType where G.Element == T>(_ makeUnderlyingGenerator: () -> G)

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> AnyGenerator<T>
}

extension AnySequence {
    func underestimateCount() -> Int
}


/// `Array` is an efficient, tail-growable random-access
/// collection of arbitrary elements.
///
/// Common Properties of Array Types
/// ================================
///
/// The information in this section applies to all three of Swift's
/// array types, `Array<T>`, `ContiguousArray<T>`, and `ArraySlice<T>`.
/// When you read the word "array" here in a normal typeface, it
/// applies to all three of them.
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
/// `Array<T>` is considered bridged to Objective-C iff `T` is bridged
/// to Objective-C.
///
/// When `T` is a `class` or `@objc` protocol type, `Array` may store
/// its elements in an `NSArray`.  Since any arbitrary subclass of
/// `NSArray` can become an `Array`, there are no guarantees about
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
/// When `T` is a `class` or `@objc` protocol, bridging takes O(1)
/// time and O(1) space.  Other `Array`s must be bridged
/// element-by-element, allocating a new object for each element, at a
/// cost of at least O(`count`) time and space.
///
/// Bridging from Objective-C
/// -------------------------
///
/// An `NSArray` can be implicitly or explicitly converted to any
/// bridged `Array<T>`.  This conversion calls `copyWithZone` on the
/// `NSArray`, to ensure it won't be modified, and stores the result
/// in the `Array`.  Type-checking, to ensure the `NSArray`'s
/// elements match or can be bridged to `T`, is deferred until the
/// first element access.
struct Array<T> : CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType, MutableCollectionType, Sliceable, _Sliceable, _DestructorSafeContainer {

    /// The type of element stored by this `Array`.
    typealias Element = T

    /// Always zero, which is the index of the first element when non-empty.
    var startIndex: Int { get }

    /// A "past-the-end" element index; the successor of the last valid
    /// subscript argument.
    var endIndex: Int { get }
    subscript (index: Int) -> T

    /// Return a *generator* over the elements.
    ///
    /// - Complexity: O(1).
    func generate() -> IndexingGenerator<[T]>

    /// A type that can represent a sub-range of an `Array`.
    typealias SubSlice = ArraySlice<T>
    subscript (subRange: Range<Int>) -> ArraySlice<T>
}

extension Array : __ArrayType {
}

extension Array : ArrayLiteralConvertible {

    /// Create an instance containing `elements`.
    init(arrayLiteral elements: T...)
}

extension Array : _ArrayType, MutableSliceable, RangeReplaceableCollectionType, ExtensibleCollectionType {

    /// Construct an empty Array.
    init()

    /// Construct from an arbitrary sequence with elements of type `T`.
    init<S : SequenceType where S.Generator.Element == _Buffer.Element>(_ s: S)

    /// Construct a Array of `count` elements, each initialized to
    /// `repeatedValue`.
    init(count: Int, repeatedValue: T)

    /// The number of elements the Array stores.
    var count: Int { get }

    /// The number of elements the `Array` can store without reallocation.
    var capacity: Int { get }

    /// Reserve enough space to store `minimumCapacity` elements.
    ///
    /// - Postcondition: `capacity >= minimumCapacity` and the array has
    ///   mutable contiguous storage.
    ///
    /// - Complexity: O(`count`).
    mutating func reserveCapacity(minimumCapacity: Int)

    /// Append `newElement` to the Array.
    ///
    /// - Complexity: Amortized O(1) unless `self`'s storage is shared with another live array; O(`count`) if `self` does not wrap a bridged `NSArray`; otherwise the efficiency is unspecified..
    mutating func append(newElement: T)

    /// Append the elements of `newElements` to `self`.
    ///
    /// - Complexity: O(*length of result*).
    mutating func extend<S : SequenceType where S.Generator.Element == T>(newElements: S)

    /// Remove an element from the end of the Array in O(1).
    ///
    /// - Requires: `count > 0`.
    mutating func removeLast() -> T

    /// Insert `newElement` at index `i`.
    ///
    /// - Requires: `i <= count`.
    ///
    /// - Complexity: O(`count`).
    mutating func insert(newElement: T, atIndex i: Int)

    /// Remove and return the element at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`count`).
    mutating func removeAtIndex(index: Int) -> T

    /// Remove all elements.
    ///
    /// - Postcondition: `capacity == 0` iff `keepCapacity` is `false`.
    ///
    /// - Complexity: O(`self.count`).
    mutating func removeAll(keepCapacity keepCapacity: Bool = default)

    /// Interpose `self` between each consecutive pair of `elements`,
    /// and concatenate the elements of the resulting sequence.  For
    /// example, `[-1, -2].join([[1, 2, 3], [4, 5, 6], [7, 8, 9]])`
    /// yields `[1, 2, 3, -1, -2, 4, 5, 6, -1, -2, 7, 8, 9]`.
    func join<S : SequenceType where S.Generator.Element == Array<T>>(elements: S) -> [T]
}

extension Array : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension Array : CustomStringConvertible, CustomDebugStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }
}



extension Array {

    /// Call `body(p)`, where `p` is a pointer to the `Array`'s
    /// contiguous storage. If no such storage exists, it is first created.
    ///
    /// Often, the optimizer can eliminate bounds checks within an
    /// array algorithm, but when that fails, invoking the
    /// same algorithm on `body`'s argument lets you trade safety for
    /// speed.
    func withUnsafeBufferPointer<R>(@noescape body: (UnsafeBufferPointer<T>) -> R) -> R

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
    mutating func withUnsafeMutableBufferPointer<R>(@noescape body: (inout UnsafeMutableBufferPointer<T>) -> R) -> R
}

extension Array {

    /// Replace the given `subRange` of elements with `newElements`.
    ///
    /// - Complexity: O(`subRange.count`) if `subRange.endIndex
    ///   == self.endIndex` and `isEmpty(newElements)`, O(N) otherwise.
    mutating func replaceRange<C : CollectionType where C.Generator.Element == _Buffer.Element>(subRange: Range<Int>, with newElements: C)

    /// Insert `newElements` at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`count + newElements.count`).
    mutating func splice<S : CollectionType where S.Generator.Element == T>(newElements: S, atIndex i: Int)

    /// Remove the indicated `subRange` of elements.
    ///
    /// - Complexity: O(`count`).
    mutating func removeRange(subRange: Range<Int>)
}




/// Conforming types can be initialized with array literals.
protocol ArrayLiteralConvertible {
    typealias Element

    /// Create an instance initialized with `elements`.
    init(arrayLiteral elements: Self.Element...)
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
struct ArraySlice<T> : _CollectionDefaultsType, SequenceType, _CollectionGeneratorDefaultsType, CollectionType, MutableCollectionType, Sliceable, _Sliceable, _DestructorSafeContainer {

    /// The type of element stored by this `ArraySlice`.
    typealias Element = T

    /// Always zero, which is the index of the first element when non-empty.
    var startIndex: Int { get }

    /// A "past-the-end" element index; the successor of the last valid
    /// subscript argument.
    var endIndex: Int { get }
    subscript (index: Int) -> T

    /// Return a *generator* over the elements.
    ///
    /// - Complexity: O(1).
    func generate() -> IndexingGenerator<ArraySlice<T>>

    /// A type that can represent a sub-range of an `ArraySlice`.
    typealias SubSlice = ArraySlice<T>
    subscript (subRange: Range<Int>) -> ArraySlice<T>
}

extension ArraySlice : __ArrayType {
}

extension ArraySlice : ArrayLiteralConvertible {

    /// Create an instance containing `elements`.
    init(arrayLiteral elements: T...)
}

extension ArraySlice : _ArrayType, MutableSliceable, RangeReplaceableCollectionType, ExtensibleCollectionType {

    /// Construct an empty ArraySlice.
    init()

    /// Construct from an arbitrary sequence with elements of type `T`.
    init<S : SequenceType where S.Generator.Element == _Buffer.Element>(_ s: S)

    /// Construct a ArraySlice of `count` elements, each initialized to
    /// `repeatedValue`.
    init(count: Int, repeatedValue: T)

    /// The number of elements the ArraySlice stores.
    var count: Int { get }

    /// The number of elements the `ArraySlice` can store without reallocation.
    var capacity: Int { get }

    /// Reserve enough space to store `minimumCapacity` elements.
    ///
    /// - Postcondition: `capacity >= minimumCapacity` and the array has
    ///   mutable contiguous storage.
    ///
    /// - Complexity: O(`count`).
    mutating func reserveCapacity(minimumCapacity: Int)

    /// Append `newElement` to the ArraySlice.
    ///
    /// - Complexity: Amortized O(1) unless `self`'s storage is shared with another live array; O(`count`) otherwise..
    mutating func append(newElement: T)

    /// Append the elements of `newElements` to `self`.
    ///
    /// - Complexity: O(*length of result*).
    mutating func extend<S : SequenceType where S.Generator.Element == T>(newElements: S)

    /// Remove an element from the end of the ArraySlice in O(1).
    ///
    /// - Requires: `count > 0`.
    mutating func removeLast() -> T

    /// Insert `newElement` at index `i`.
    ///
    /// - Requires: `i <= count`.
    ///
    /// - Complexity: O(`count`).
    mutating func insert(newElement: T, atIndex i: Int)

    /// Remove and return the element at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`count`).
    mutating func removeAtIndex(index: Int) -> T

    /// Remove all elements.
    ///
    /// - Postcondition: `capacity == 0` iff `keepCapacity` is `false`.
    ///
    /// - Complexity: O(`self.count`).
    mutating func removeAll(keepCapacity keepCapacity: Bool = default)

    /// Interpose `self` between each consecutive pair of `elements`,
    /// and concatenate the elements of the resulting sequence.  For
    /// example, `[-1, -2].join([[1, 2, 3], [4, 5, 6], [7, 8, 9]])`
    /// yields `[1, 2, 3, -1, -2, 4, 5, 6, -1, -2, 7, 8, 9]`.
    func join<S : SequenceType where S.Generator.Element == ArraySlice<T>>(elements: S) -> ArraySlice<T>
}

extension ArraySlice : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension ArraySlice : CustomStringConvertible, CustomDebugStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }
}



extension ArraySlice {

    /// Call `body(p)`, where `p` is a pointer to the `ArraySlice`'s
    /// contiguous storage.
    ///
    /// Often, the optimizer can eliminate bounds checks within an
    /// array algorithm, but when that fails, invoking the
    /// same algorithm on `body`'s argument lets you trade safety for
    /// speed.
    func withUnsafeBufferPointer<R>(@noescape body: (UnsafeBufferPointer<T>) -> R) -> R

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
    mutating func withUnsafeMutableBufferPointer<R>(@noescape body: (inout UnsafeMutableBufferPointer<T>) -> R) -> R
}

extension ArraySlice {

    /// Replace the given `subRange` of elements with `newElements`.
    ///
    /// - Complexity: O(`subRange.count`) if `subRange.endIndex
    ///   == self.endIndex` and `isEmpty(newElements)`, O(N) otherwise.
    mutating func replaceRange<C : CollectionType where C.Generator.Element == _Buffer.Element>(subRange: Range<Int>, with newElements: C)

    /// Insert `newElements` at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`count + newElements.count`).
    mutating func splice<S : CollectionType where S.Generator.Element == T>(newElements: S, atIndex i: Int)

    /// Remove the indicated `subRange` of elements.
    ///
    /// - Complexity: O(`count`).
    mutating func removeRange(subRange: Range<Int>)
}


/// A mutable pointer-to-ObjC-pointer argument.
///
/// This type has implicit conversions to allow passing any of the following
/// to a C or ObjC API:
///
/// - `nil`, which gets passed as a null pointer,
/// - an inout argument of the referenced type, which gets passed as a pointer
///   to a writeback temporary with autoreleasing ownership semantics,
/// - an `UnsafeMutablePointer<T>`, which is passed as-is.
///
/// Passing pointers to mutable arrays of ObjC class pointers is not
/// directly supported. Unlike `UnsafeMutablePointer<T>`,
/// AutoreleasingUnsafeMutablePointer must reference storage that does
/// not own a reference count to the referenced
/// value. UnsafeMutablePointer's operations, by contrast, assume that
/// the referenced storage owns values loaded from or stored to it.
///
/// This type does not carry an owner pointer unlike the other C*Pointer types
/// because it only needs to reference the results of inout conversions, which
/// already have writeback-scoped lifetime.
struct AutoreleasingUnsafeMutablePointer<T> : Equatable, NilLiteralConvertible, _PointerType {

    /// Access the underlying raw memory, getting and
    /// setting values.
    var memory: T { get nonmutating set }
    subscript (i: Int) -> T { get }

    /// Create an instance initialized with `nil`.
    init(nilLiteral: ())

    /// Initialize to a null pointer.
    init()

    /// Explicit construction from an UnsafeMutablePointer.
    ///
    /// This is inherently unsafe; UnsafeMutablePointer assumes the
    /// referenced memory has +1 strong ownership semantics, whereas
    /// AutoreleasingUnsafeMutablePointer implies +0 semantics.
    init<U>(_ ptr: UnsafeMutablePointer<U>)
}

extension AutoreleasingUnsafeMutablePointer : CustomDebugStringConvertible {

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }
}

extension AutoreleasingUnsafeMutablePointer : CVarArgType {
}


/// An *index* that can step backwards via application of its
/// `predecessor()` method.
protocol BidirectionalIndexType : ForwardIndexType {

    /// Return the previous consecutive value in a discrete sequence.
    ///
    /// If `self` has a well-defined successor,
    /// `self.successor().predecessor() == self`.  If `self` has a
    /// well-defined predecessor, `self.predecessor().successor() ==
    /// self`.
    ///
    /// - Requires: `self` has a well-defined predecessor.
    func predecessor() -> Self
}




/// The lazy `CollectionType` returned by `reverse(c)` where `c` is a
/// `CollectionType` with an `Index` conforming to `BidirectionalIndexType`.
struct BidirectionalReverseView<T : CollectionType where T.Index : BidirectionalIndexType> : CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType {

    /// A type that represents a valid position in the collection.
    ///
    /// Valid indices consist of the position of every element and a
    /// "past the end" position that's not valid for use as a subscript.
    typealias Index = ReverseBidirectionalIndex<T.Index>

    /// A type whose instances can produce the elements of this
    /// sequence, in order.
    typealias Generator = IndexingGenerator<BidirectionalReverseView<T>>

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> IndexingGenerator<BidirectionalReverseView<T>>

    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    var startIndex: ReverseBidirectionalIndex<T.Index> { get }

    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    var endIndex: ReverseBidirectionalIndex<T.Index> { get }
    subscript (position: ReverseBidirectionalIndex<T.Index>) -> T.Generator.Element { get }
}


/// A `RandomAccessIndexType` that has two possible values.  Used as
/// the `Index` type for `CollectionOfOne<T>`.
enum Bit : Int, Comparable, RandomAccessIndexType, BidirectionalIndexType, ForwardIndexType, _Incrementable, Strideable, _Strideable, Reflectable {
    case Zero
    case One

    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: `self == .Zero`.
    func successor() -> Bit

    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: `self != .Zero`.
    func predecessor() -> Bit

    /// Return the minimum number of applications of `successor` or
    /// `predecessor` required to reach `other` from `self`.
    ///
    /// - Complexity: O(1).
    func distanceTo(other: Bit) -> Int

    /// Return `self` offset by `n` steps.
    ///
    /// - Returns: If `n > 0`, the result of applying `successor` to
    ///   `self` `n` times.  If `n < 0`, the result of applying
    ///   `predecessor` to `self` `-n` times. Otherwise, `self`.
    ///
    /// - Complexity: O(1).
    func advancedBy(distance: Int) -> Bit

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension Bit : IntegerArithmeticType, _IntegerArithmeticType {

    /// Add `lhs` and `rhs`, returning a result and a `Bool` that is
    /// true iff the operation caused an arithmetic overflow.
    static func addWithOverflow(lhs: Bit, _ rhs: Bit) -> (Bit, overflow: Bool)

    /// Subtract `lhs` and `rhs`, returning a result and a `Bool` that is
    /// true iff the operation caused an arithmetic overflow.
    static func subtractWithOverflow(lhs: Bit, _ rhs: Bit) -> (Bit, overflow: Bool)

    /// Multiply `lhs` and `rhs`, returning a result and a `Bool` that is
    /// true iff the operation caused an arithmetic overflow.
    static func multiplyWithOverflow(lhs: Bit, _ rhs: Bit) -> (Bit, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning a result and a `Bool` that is
    /// true iff the operation caused an arithmetic overflow.
    static func divideWithOverflow(lhs: Bit, _ rhs: Bit) -> (Bit, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning the remainder and a `Bool` that is
    /// true iff the operation caused an arithmetic overflow.
    static func remainderWithOverflow(lhs: Bit, _ rhs: Bit) -> (Bit, overflow: Bool)

    /// Represent this number using Swift's widest native signed integer
    /// type.
    func toIntMax() -> IntMax
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
protocol BitwiseOperationsType {

    /// Returns the intersection of bits set in `lhs` and `rhs`.
    ///
    /// - Complexity: O(1).
    func &(lhs: Self, rhs: Self) -> Self

    /// Returns the union of bits set in `lhs` and `rhs`.
    ///
    /// - Complexity: O(1).
    func |(lhs: Self, rhs: Self) -> Self

    /// Returns the bits that are set in exactly one of `lhs` and `rhs`.
    ///
    /// - Complexity: O(1).
    func ^(lhs: Self, rhs: Self) -> Self

    /// Returns `x ^ ~Self.allZeros`.
    ///
    /// - Complexity: O(1).
    prefix func ~(x: Self) -> Self

    /// The empty bitset.
    ///
    /// Also the [identity element](http://en.wikipedia.org/wiki/Identity_element) for `|` and
    /// `^`, and the [fixed point](http://en.wikipedia.org/wiki/Fixed_point_(mathematics)) for
    /// `&`.
    static var allZeros: Self { get }
}


/// A value type whose instances are either `true` or `false`.
struct Bool {

    /// Default-initialize Boolean value to `false`.
    init()
}

extension Bool : BooleanLiteralConvertible {
    init(_builtinBooleanLiteral value: Builtin.Int1)

    /// Create an instance initialized to `value`.
    init(booleanLiteral value: Bool)
}

extension Bool : BooleanType {

    /// Identical to `self`.
    var boolValue: Bool { get }

    /// Construct an instance representing the same logical value as
    /// `value`.
    init<T : BooleanType>(_ value: T)
}

extension Bool : CustomStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }
}

extension Bool : Equatable, Hashable {

    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: the hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    var hashValue: Int { get }
}

extension Bool : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}


/// Conforming types can be initialized with the Boolean literals
/// `true` and `false`.
protocol BooleanLiteralConvertible {
    typealias BooleanLiteralType

    /// Create an instance initialized to `value`.
    init(booleanLiteral value: Self.BooleanLiteralType)
}


/// The default type for an otherwise-unconstrained Boolean literal.
typealias BooleanLiteralType = Bool


/// A type that represents a Boolean value.
///
/// Types that conform to the `BooleanType` protocol can be used as
/// the condition in control statements (`if`, `while`, C-style `for`)
/// and other logical value contexts (e.g., `case` statement guards).
///
/// Only two types provided by Swift, `Bool` and `ObjCBool`, conform
/// to `BooleanType`. Expanding this set to include types that
/// represent more than simple boolean values is discouraged.
protocol BooleanType {

    /// The value of `self`, expressed as a `Bool`.
    var boolValue: Bool { get }
}


/// The C '_Bool' and C++ 'bool' type.
typealias CBool = Bool


/// The C 'char' type.
///
/// This will be the same as either `CSignedChar` (in the common
/// case) or `CUnsignedChar`, depending on the platform.
typealias CChar = Int8


/// The C++11 'char16_t' type, which has UTF-16 encoding.
typealias CChar16 = UInt16


/// The C++11 'char32_t' type, which has UTF-32 encoding.
typealias CChar32 = UnicodeScalar


/// The C 'double' type.
typealias CDouble = Double


/// The C 'float' type.
typealias CFloat = Float


/// The C 'int' type.
typealias CInt = Int32


/// The C 'long' type.
typealias CLong = Int


/// The C 'long long' type.
typealias CLongLong = Int64


/// A wrapper around an opaque C pointer.
///
/// Opaque pointers are used to represent C pointers to types that
/// cannot be represented in Swift, such as incomplete struct types.
struct COpaquePointer : Equatable, Hashable, NilLiteralConvertible {

    /// Construct a `nil` instance.
    init()

    /// Construct a `COpaquePointer` from a given address in memory.
    ///
    /// This is a fundamentally unsafe conversion.
    init(bitPattern: Word)

    /// Construct a `COpaquePointer` from a given address in memory.
    ///
    /// This is a fundamentally unsafe conversion.
    init(bitPattern: UWord)

    /// Convert a typed `UnsafePointer` to an opaque C pointer.
    init<T>(_ source: UnsafePointer<T>)

    /// Convert a typed `UnsafeMutablePointer` to an opaque C pointer.
    init<T>(_ source: UnsafeMutablePointer<T>)

    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    var hashValue: Int { get }

    /// Create an instance initialized with `nil`.
    init(nilLiteral: ())
}

extension COpaquePointer : CustomDebugStringConvertible {

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }
}

extension COpaquePointer : CVarArgType {
}


/// The C 'short' type.
typealias CShort = Int16


/// The C 'signed char' type.
typealias CSignedChar = Int8


/// The C 'unsigned char' type.
typealias CUnsignedChar = UInt8


/// The C 'unsigned int' type.
typealias CUnsignedInt = UInt32


/// The C 'unsigned long' type.
typealias CUnsignedLong = UInt


/// The C 'unsigned long long' type.
typealias CUnsignedLongLong = UInt64


/// The C 'unsigned short' type.
typealias CUnsignedShort = UInt16


/// The corresponding Swift type to `va_list` in imported C APIs.
struct CVaListPointer {
    init(_fromUnsafeMutablePointer from: UnsafeMutablePointer<Void>)
}

extension CVaListPointer : CustomDebugStringConvertible {

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }
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
protocol CVarArgType {
}


/// The C++ 'wchar_t' type.
typealias CWideChar = UnicodeScalar


/// `Character` represents some Unicode grapheme cluster as
/// defined by a canonical, localized, or otherwise tailored
/// segmentation algorithm.
struct Character : ExtendedGraphemeClusterLiteralConvertible, UnicodeScalarLiteralConvertible, Equatable, Hashable, Comparable {

    /// Construct a `Character` containing just the given `scalar`.
    init(_ scalar: UnicodeScalar)
    init(_builtinUnicodeScalarLiteral value: Builtin.Int32)

    /// Create an instance initialized to `value`.
    init(unicodeScalarLiteral value: Character)
    init(_builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer, byteSize: Builtin.Word, isASCII: Builtin.Int1)

    /// Create an instance initialized to `value`.
    init(extendedGraphemeClusterLiteral value: Character)

    /// Create an instance from a single-character `String`.
    ///
    /// - Requires: `s` contains exactly one extended grapheme cluster.
    init(_ s: String)

    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    var hashValue: Int { get }
}

extension Character : CustomDebugStringConvertible {

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }
}

extension Character : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension Character : Streamable {

    /// Write a textual representation of `self` into `target`.
    func writeTo<Target : OutputStreamType>(inout target: Target)
}


/// A closed `IntervalType`, which contains both its `start` and its
/// `end`.  Cannot represent an empty interval.
struct ClosedInterval<T : Comparable> : IntervalType, Equatable, CustomStringConvertible, CustomDebugStringConvertible, Reflectable {

    /// The type of the `Interval`'s endpoints.
    typealias Bound = T

    /// Construct a copy of `x`.
    init(_ x: ClosedInterval<T>)

    /// Construct an interval with the given bounds.
    ///
    /// - Requires: `start <= end`.
    init(_ start: T, _ end: T)

    /// The `Interval`'s lower bound.
    ///
    /// Invariant: `start` <= `end`.
    var start: T { get }

    /// The `Interval`'s upper bound.
    ///
    /// Invariant: `start` <= `end`.
    var end: T { get }

    /// A textual representation of `self`.
    var description: String { get }

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }

    /// Returns `true` iff the `Interval` contains `x`.
    func contains(x: T) -> Bool

    /// Returns `intervalToClamp` clamped to `self`.
    ///
    /// The bounds of the result, even if it is empty, are always limited to the bounds of
    /// `self`.
    func clamp(intervalToClamp: ClosedInterval<T>) -> ClosedInterval<T>

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension ClosedInterval {

    /// `true` iff the `Interval` is empty.  In the case of
    /// `ClosedInterval`, always returns `false`.
    var isEmpty: Bool { get }
}


/// A collection containing a single element of type `T`.
struct CollectionOfOne<T> : CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType {

    /// A type that represents a valid position in the collection.
    ///
    /// Valid indices consist of the position of every element and a
    /// "past the end" position that's not valid for use as a subscript.
    typealias Index = Bit

    /// Construct an instance containing just `element`.
    init(_ element: T)

    /// The position of the first element.
    var startIndex: Index { get }

    /// The "past the end" position; always identical to
    /// `startIndex.successor()`.
    ///
    /// - Note: `endIndex` is not a valid argument to `subscript`.
    var endIndex: Index { get }

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> GeneratorOfOne<T>
    subscript (position: Index) -> T { get }

    /// Return the number of elements (always one).
    var count: Int { get }
}

extension CollectionOfOne : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
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
protocol CollectionType : SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType {
    subscript (position: Self.Index) -> Self.Generator.Element { get }

    /// Returns `true` iff `self` is empty.
    var isEmpty: Bool { get }

    /// Returns the number of elements.
    ///
    /// - Complexity: O(1) if `Index` conforms to `RandomAccessIndexType`;
    ///   O(N) otherwise.
    var count: Self.Index.Distance { get }
}

extension CollectionType {

    /// Return an `Array` containing the results of mapping `transform`
    /// over `self`.
    ///
    /// - Complexity: O(N).
    func map<T>(@noescape transform: (Self.Generator.Element) -> T) -> [T]
}

extension CollectionType {

    /// Returns an `Array` containing the elements of `self`,
    /// in order, that satisfy the predicate `includeElement`.
    func filter(@noescape includeElement: (Self.Generator.Element) -> Bool) -> [Self.Generator.Element]
}

extension CollectionType {

    /// Returns the first element of `self`, or `nil` if `self` is empty.
    var first: Self.Generator.Element? { get }
}



extension CollectionType where Index : BidirectionalIndexType {
    var last: Self.Generator.Element? { get }
}

extension CollectionType where Generator.Element : Equatable {

    /// Returns the first index where `value` appears in `self` or `nil` if
    /// `value` is not found.
    ///
    /// - Complexity: O(`self.count`).
    func indexOf(element: Self.Generator.Element) -> Self.Index?
}

extension CollectionType {

    /// Returns the first index where `predicate` returns `true` for the
    /// corresponding value, or `nil` if such value is not found.
    ///
    /// - Complexity: O(`self.count`).
    func indexOf(@noescape predicate: (Self.Generator.Element) -> Bool) -> Self.Index?
}

extension CollectionType {

    /// Return the range of valid index values.
    ///
    /// The result's `endIndex` is the same as that of `self`.  Because
    /// `Range` is half-open, iterating the values of the result produces
    /// all valid subscript arguments for `self`, omitting its `endIndex`.
    var indices: Range<Self.Index> { get }
}

extension CollectionType where Index : BidirectionalIndexType {

    /// Return a lazy `CollectionType` containing the elements of `self`
    /// in reverse order.
    func reverse() -> BidirectionalReverseView<Self>
}

extension CollectionType where Index : RandomAccessIndexType {

    /// Return a lazy `CollectionType` containing the elements of `self`
    /// in reverse order.
    func reverse() -> RandomAccessReverseView<Self>
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
protocol Comparable : Equatable {

    /// A [strict total order](http://en.wikipedia.org/wiki/Total_order#Strict_total_order)
    /// over instances of `Self`.
    func <(lhs: Self, rhs: Self) -> Bool
    func <=(lhs: Self, rhs: Self) -> Bool
    func >=(lhs: Self, rhs: Self) -> Bool
    func >(lhs: Self, rhs: Self) -> Bool
}


/// A fast, contiguously-stored array of `T`.
///
/// Efficiency is equivalent to that of `Array`, unless `T` is a
/// `class` or `@objc` `protocol` type, in which case using
/// `ContiguousArray` may be more efficient.  Note, however, that
/// `ContiguousArray` does not bridge to Objective-C.  See `Array`,
/// with which `ContiguousArray` shares most properties, for more
/// detail.
struct ContiguousArray<T> : CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType, MutableCollectionType, Sliceable, _Sliceable, _DestructorSafeContainer {

    /// The type of element stored by this `ContiguousArray`.
    typealias Element = T

    /// Always zero, which is the index of the first element when non-empty.
    var startIndex: Int { get }

    /// A "past-the-end" element index; the successor of the last valid
    /// subscript argument.
    var endIndex: Int { get }
    subscript (index: Int) -> T

    /// Return a *generator* over the elements.
    ///
    /// - Complexity: O(1).
    func generate() -> IndexingGenerator<ContiguousArray<T>>

    /// A type that can represent a sub-range of a ContiguousArray.
    typealias SubSlice = ArraySlice<T>
    subscript (subRange: Range<Int>) -> ArraySlice<T>
}

extension ContiguousArray : __ArrayType {
}

extension ContiguousArray : ArrayLiteralConvertible {

    /// Create an instance containing `elements`.
    init(arrayLiteral elements: T...)
}

extension ContiguousArray : _ArrayType, MutableSliceable, RangeReplaceableCollectionType, ExtensibleCollectionType {

    /// Construct an empty ContiguousArray.
    init()

    /// Construct from an arbitrary sequence with elements of type `T`.
    init<S : SequenceType where S.Generator.Element == _Buffer.Element>(_ s: S)

    /// Construct a ContiguousArray of `count` elements, each initialized to
    /// `repeatedValue`.
    init(count: Int, repeatedValue: T)

    /// The number of elements the ContiguousArray stores.
    var count: Int { get }

    /// The number of elements the `ContiguousArray` can store without reallocation.
    var capacity: Int { get }

    /// Reserve enough space to store `minimumCapacity` elements.
    ///
    /// - Postcondition: `capacity >= minimumCapacity` and the array has
    ///   mutable contiguous storage.
    ///
    /// - Complexity: O(`count`).
    mutating func reserveCapacity(minimumCapacity: Int)

    /// Append `newElement` to the ContiguousArray.
    ///
    /// - Complexity: Amortized O(1) unless `self`'s storage is shared with another live array; O(`count`) otherwise..
    mutating func append(newElement: T)

    /// Append the elements of `newElements` to `self`.
    ///
    /// - Complexity: O(*length of result*).
    mutating func extend<S : SequenceType where S.Generator.Element == T>(newElements: S)

    /// Remove an element from the end of the ContiguousArray in O(1).
    ///
    /// - Requires: `count > 0`.
    mutating func removeLast() -> T

    /// Insert `newElement` at index `i`.
    ///
    /// - Requires: `i <= count`.
    ///
    /// - Complexity: O(`count`).
    mutating func insert(newElement: T, atIndex i: Int)

    /// Remove and return the element at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`count`).
    mutating func removeAtIndex(index: Int) -> T

    /// Remove all elements.
    ///
    /// - Postcondition: `capacity == 0` iff `keepCapacity` is `false`.
    ///
    /// - Complexity: O(`self.count`).
    mutating func removeAll(keepCapacity keepCapacity: Bool = default)

    /// Interpose `self` between each consecutive pair of `elements`,
    /// and concatenate the elements of the resulting sequence.  For
    /// example, `[-1, -2].join([[1, 2, 3], [4, 5, 6], [7, 8, 9]])`
    /// yields `[1, 2, 3, -1, -2, 4, 5, 6, -1, -2, 7, 8, 9]`.
    func join<S : SequenceType where S.Generator.Element == ContiguousArray<T>>(elements: S) -> ContiguousArray<T>
}

extension ContiguousArray : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension ContiguousArray : CustomStringConvertible, CustomDebugStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }
}



extension ContiguousArray {

    /// Call `body(p)`, where `p` is a pointer to the `ContiguousArray`'s
    /// contiguous storage.
    ///
    /// Often, the optimizer can eliminate bounds checks within an
    /// array algorithm, but when that fails, invoking the
    /// same algorithm on `body`'s argument lets you trade safety for
    /// speed.
    func withUnsafeBufferPointer<R>(@noescape body: (UnsafeBufferPointer<T>) -> R) -> R

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
    mutating func withUnsafeMutableBufferPointer<R>(@noescape body: (inout UnsafeMutableBufferPointer<T>) -> R) -> R
}

extension ContiguousArray {

    /// Replace the given `subRange` of elements with `newElements`.
    ///
    /// - Complexity: O(`subRange.count`) if `subRange.endIndex
    ///   == self.endIndex` and `isEmpty(newElements)`, O(N) otherwise.
    mutating func replaceRange<C : CollectionType where C.Generator.Element == _Buffer.Element>(subRange: Range<Int>, with newElements: C)

    /// Insert `newElements` at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`count + newElements.count`).
    mutating func splice<S : CollectionType where S.Generator.Element == T>(newElements: S, atIndex i: Int)

    /// Remove the indicated `subRange` of elements.
    ///
    /// - Complexity: O(`count`).
    mutating func removeRange(subRange: Range<Int>)
}


/// A type with a customized textual representation suitable for
/// debugging purposes.
///
/// This textual representation is used when values are written to an
/// *output stream* by `debugPrint` and `debugPrintln`, and is
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
protocol CustomDebugStringConvertible {

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }
}


/// A type that explicitly supplies its own Mirror but whose
/// descendant classes are not represented in the Mirror unless they
/// also override `customMirror()`.
protocol CustomLeafReflectable : CustomReflectable {
}


/// A type that explicitly supplies its own PlaygroundQuickLook.
///
/// Instances of any type can be `reflect`'ed upon, but if you are
/// not satisfied with the `Mirror` supplied for your type by default,
/// you can make it conform to `CustomReflectable` and return a custom
/// `Mirror`.
protocol CustomPlaygroundQuickLookable {

    /// Return the `Mirror` for `self`.
    ///
    /// - Note: If `Self` has value semantics, the `Mirror` should be
    ///   unaffected by subsequent mutations of `self`.
    func customPlaygroundQuickLook() -> PlaygroundQuickLook
}


/// A type that explicitly supplies its own Mirror.
///
/// Instances of any type can be `reflect`'ed upon, but if you are
/// not satisfied with the `Mirror` supplied for your type by default,
/// you can make it conform to `CustomReflectable` and return a custom
/// `Mirror`.
protocol CustomReflectable {

    /// Return the `Mirror` for `self`.
    ///
    /// - Note: If `Self` has value semantics, the `Mirror` should be
    ///   unaffected by subsequent mutations of `self`.
    func customMirror() -> Mirror
}


/// A type with a customized textual representation.
///
/// This textual representation is used when values are written to an
/// *output stream*, for example, by `print`.
///
/// - Note: `String(instance)` will work for an `instance` of *any*
///   type, returning its `description` if the `instance` happens to be
///   `CustomStringConvertible`.  Using `CustomStringConvertible` as a
/// generic constraint, or accessing a conforming type's `description`
/// directly, is therefore discouraged.
///
/// - SeeAlso: `String.init<T>(T)`, `CustomDebugStringConvertible`
protocol CustomStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }
}


/// A hash-based mapping from `Key` to `Value` instances.  Also a
/// collection of key-value pairs with no defined ordering.
struct Dictionary<Key : Hashable, Value> : CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType, DictionaryLiteralConvertible {
    typealias Element = (Key, Value)
    typealias Index = DictionaryIndex<Key, Value>

    /// Create an empty dictionary.
    init()

    /// Create a dictionary with at least the given number of
    /// elements worth of storage.  The actual capacity will be the
    /// smallest power of 2 that's >= `minimumCapacity`.
    init(minimumCapacity: Int)

    /// The position of the first element in a non-empty dictionary.
    ///
    /// Identical to `endIndex` in an empty dictionary.
    ///
    /// - Complexity: Amortized O(1) if `self` does not wrap a bridged
    ///   `NSDictionary`, O(N) otherwise.
    var startIndex: DictionaryIndex<Key, Value> { get }

    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    ///
    /// - Complexity: Amortized O(1) if `self` does not wrap a bridged
    ///   `NSDictionary`, O(N) otherwise.
    var endIndex: DictionaryIndex<Key, Value> { get }

    /// Returns the `Index` for the given key, or `nil` if the key is not
    /// present in the dictionary.
    func indexForKey(key: Key) -> DictionaryIndex<Key, Value>?
    subscript (position: DictionaryIndex<Key, Value>) -> (Key, Value) { get }
    subscript (key: Key) -> Value?

    /// Update the value stored in the dictionary for the given key, or, if they
    /// key does not exist, add a new key-value pair to the dictionary.
    ///
    /// Returns the value that was replaced, or `nil` if a new key-value pair
    /// was added.
    mutating func updateValue(value: Value, forKey key: Key) -> Value?

    /// Remove the key-value pair at `index`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`count`).
    mutating func removeAtIndex(index: DictionaryIndex<Key, Value>)

    /// Remove a given key and the associated value from the dictionary.
    /// Returns the value that was removed, or `nil` if the key was not present
    /// in the dictionary.
    mutating func removeValueForKey(key: Key) -> Value?

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
    mutating func removeAll(keepCapacity keepCapacity: Bool = default)

    /// The number of entries in the dictionary.
    ///
    /// - Complexity: O(1).
    var count: Int { get }

    /// Return a *generator* over the (key, value) pairs.
    ///
    /// - Complexity: O(1).
    func generate() -> DictionaryGenerator<Key, Value>

    /// Create an instance initialized with `elements`.
    init(dictionaryLiteral elements: (Key, Value)...)

    /// A collection containing just the keys of `self`.
    ///
    /// Keys appear in the same order as they occur as the `.0` member
    /// of key-value pairs in `self`.  Each key in the result has a
    /// unique value.
    var keys: LazyForwardCollection<MapCollectionView<[Key : Value], Key>> { get }

    /// A collection containing just the values of `self`.
    ///
    /// Values appear in the same order as they occur as the `.1` member
    /// of key-value pairs in `self`.
    var values: LazyForwardCollection<MapCollectionView<[Key : Value], Value>> { get }

    /// `true` iff `count == 0`.
    var isEmpty: Bool { get }
}

extension Dictionary : CustomStringConvertible, CustomDebugStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }
}

extension Dictionary : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}




/// A generator over the members of a `Dictionary<Key, Value>`.
struct DictionaryGenerator<Key : Hashable, Value> : GeneratorType {

    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: No preceding call to `self.next()` has returned `nil`.
    mutating func next() -> (Key, Value)?
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
struct DictionaryIndex<Key : Hashable, Value> : ForwardIndexType, _Incrementable, Equatable, Comparable {

    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    func successor() -> DictionaryIndex<Key, Value>
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
struct DictionaryLiteral<Key, Value> : DictionaryLiteralConvertible {

    /// Store `elements`.
    init(dictionaryLiteral elements: (Key, Value)...)
}

extension DictionaryLiteral : CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType {

    /// The position of the first element in a non-empty `DictionaryLiteral`.
    ///
    /// Identical to `endIndex` in an empty `DictionaryLiteral`.
    ///
    /// - Complexity: O(1).
    var startIndex: Int { get }

    /// The `DictionaryLiteral`'s "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    ///
    /// - Complexity: O(1).
    var endIndex: Int { get }
    subscript (position: Int) -> (Key, Value) { get }

    /// Return a *generator* over the elements of this *sequence*.  The
    /// *generator*'s next element is the first element of the
    /// sequence.
    ///
    /// - Complexity: O(1).
    func generate() -> IndexingGenerator<DictionaryLiteral<Key, Value>>
}


/// Conforming types can be initialized with dictionary literals.
protocol DictionaryLiteralConvertible {
    typealias Key
    typealias Value

    /// Create an instance initialized with `elements`.
    init(dictionaryLiteral elements: (Self.Key, Self.Value)...)
}

struct Double {

    /// Create an instance initialized to zero.
    init()
    init(_bits v: Builtin.FPIEEE64)

    /// Create an instance initialized to `value`.
    init(_ value: Double)
}

extension Double : CustomStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }
}

extension Double : FloatingPointType {

    /// The positive infinity.
    static var infinity: Double { get }

    /// A quiet NaN.
    static var NaN: Double { get }

    /// A quiet NaN.
    static var quietNaN: Double { get }

    /// `true` iff `self` is negative.
    var isSignMinus: Bool { get }

    /// `true` iff `self` is normal (not zero, subnormal, infinity, or
    /// NaN).
    var isNormal: Bool { get }

    /// `true` iff `self` is zero, subnormal, or normal (not infinity
    /// or NaN).
    var isFinite: Bool { get }

    /// `true` iff `self` is +0.0 or -0.0.
    var isZero: Bool { get }

    /// `true` iff `self` is subnormal.
    var isSubnormal: Bool { get }

    /// `true` iff `self` is infinity.
    var isInfinite: Bool { get }

    /// `true` iff `self` is NaN.
    var isNaN: Bool { get }

    /// `true` iff `self` is a signaling NaN.
    var isSignaling: Bool { get }
}

extension Double {

    /// The IEEE 754 "class" of this type.
    var floatingPointClass: FloatingPointClassification { get }
}

extension Double : IntegerLiteralConvertible {
    init(_builtinIntegerLiteral value: Builtin.Int2048)

    /// Create an instance initialized to `value`.
    init(integerLiteral value: Int64)
}

extension Double {
    init(_builtinFloatLiteral value: Builtin.FPIEEE80)
}

extension Double : FloatLiteralConvertible {

    /// Create an instance initialized to `value`.
    init(floatLiteral value: Double)
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
    var hashValue: Int { get }
}

extension Double : SignedNumberType, AbsoluteValuable {

    /// Returns the absolute value of `x`.
    static func abs(x: Double) -> Double
}

extension Double {
    init(_ v: UInt8)
    init(_ v: Int8)
    init(_ v: UInt16)
    init(_ v: Int16)
    init(_ v: UInt32)
    init(_ v: Int32)
    init(_ v: UInt64)
    init(_ v: Int64)
    init(_ v: UInt)
    init(_ v: Int)
}

extension Double {

    /// Construct an instance that approximates `other`.
    init(_ other: Float)

    /// Construct an instance that approximates `other`.
    init(_ other: Float80)
}

extension Double : Strideable, _Strideable {

    /// Returns a stride `x` such that `self.advancedBy(x)` approximates
    /// `other`.
    ///
    /// - Complexity: O(1).
    func distanceTo(other: Double) -> Double

    /// Returns a `Self` `x` such that `self.distanceTo(x)` approximates
    /// `n`.
    ///
    /// - Complexity: O(1).
    func advancedBy(amount: Double) -> Double
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
    init?(_ text: String)
}

extension Double : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension Double : _CVarArgPassedAsDouble, CVarArgType, _CVarArgAlignedType {
}


/// A collection whose element type is `T` but that is always empty.
struct EmptyCollection<T> : CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType {

    /// A type that represents a valid position in the collection.
    ///
    /// Valid indices consist of the position of every element and a
    /// "past the end" position that's not valid for use as a subscript.
    typealias Index = Int

    /// Construct an instance.
    init()

    /// Always zero, just like `endIndex`.
    var startIndex: Index { get }

    /// Always zero, just like `startIndex`.
    var endIndex: Index { get }

    /// Returns an empty *generator*.
    ///
    /// - Complexity: O(1).
    func generate() -> EmptyGenerator<T>
    subscript (position: Index) -> T { get }

    /// Return the number of elements (always zero).
    var count: Int { get }
}

extension EmptyCollection : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}


/// A generator that never produces an element.
///
/// - SeeAlso: `EmptyCollection<T>`.
struct EmptyGenerator<T> : GeneratorType, SequenceType {

    /// Construct an instance.
    init()

    /// `EmptyGenerator` is also a `SequenceType`, so it `generate`'s
    /// a copy of itself.
    func generate() -> EmptyGenerator<T>

    /// Return `nil`, indicating that there are no more elements.
    mutating func next() -> T?
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
struct EnumerateGenerator<Base : GeneratorType> : GeneratorType, SequenceType {

    /// The type of element returned by `next()`.
    typealias Element = (index: Int, element: Base.Element)

    /// Construct from a `Base` generator.
    init(_ base: Base)

    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: No preceding call to `self.next()` has returned `nil`.
    mutating func next() -> (index: Int, element: Base.Element)?

    /// A type whose instances can produce the elements of this
    /// sequence, in order.
    typealias Generator = EnumerateGenerator<Base>

    /// `EnumerateGenerator` is also a `SequenceType`, so it
    /// `generate`s a copy of itself.
    func generate() -> EnumerateGenerator<Base>
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
struct EnumerateSequence<Base : SequenceType> : SequenceType {

    /// Construct from a `Base` sequence.
    init(_ base: Base)

    /// Returns a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> EnumerateGenerator<Base.Generator>
}


/// Instances of conforming types can be compared for value equality
/// using operators `==` and `!=`.
///
/// When adopting `Equatable`, only the `==` operator is required to be
/// implemented.  The standard library provides an implementation for `!=`.
protocol Equatable {

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
    func ==(lhs: Self, rhs: Self) -> Bool
}

protocol ErrorType {
}


/// Conforming types can be initialized with string literals
/// containing a single [Unicode extended grapheme cluster](http://www.unicode.org/glossary/#extended_grapheme_cluster).
protocol ExtendedGraphemeClusterLiteralConvertible : UnicodeScalarLiteralConvertible {
    typealias ExtendedGraphemeClusterLiteralType

    /// Create an instance initialized to `value`.
    init(extendedGraphemeClusterLiteral value: Self.ExtendedGraphemeClusterLiteralType)
}


/// The default type for an otherwise-unconstrained Unicode extended
/// grapheme cluster literal.
typealias ExtendedGraphemeClusterType = String


/// A collection type that can be efficiently appended-to.
protocol ExtensibleCollectionType : CollectionType {

    /// Create an empty instance.
    init()

    /// A non-binding request to ensure `n` elements of available storage.
    ///
    /// This works as an optimization to avoid multiple reallocations of
    /// linear data structures like `Array`.  Conforming types may
    /// reserve more than `n`, exactly `n`, less than `n` elements of
    /// storage, or even ignore the request completely.
    mutating func reserveCapacity(n: Self.Index.Distance)

    /// Append `x` to `self`.
    ///
    /// Applying `successor()` to the index of the new element yields
    /// `self.endIndex`.
    ///
    /// - Complexity: Amortized O(1).
    mutating func append(x: Self.Generator.Element)

    /// Append the elements of `newElements` to `self`.
    ///
    /// - Complexity: O(*length of result*).
    ///
    /// A possible implementation:
    ///
    ///     reserveCapacity(self.count + newElements.underestimateCount())
    ///     for x in newElements {
    ///       self.append(x)
    ///     }
    mutating func extend<S : SequenceType where S.Generator.Element == Generator.Element>(newElements: S)
}


/// A lazy `CollectionType` wrapper that includes the elements of an
/// underlying collection that satisfy a predicate.  Not
/// automatically returned by `filter(x)` for two reasons:
///
/// * The O(1) guarantee of our `Index` would be iffy at best, since
///   it advances an underlying `Index` until the predicate is
///   satisfied.  Be aware that a `FilterCollectionView` may not offer
///   the expected efficiency for this reason.
///
/// * Constructing an `Array` from a `CollectionType` measures the length
///   of the collection before traversing it to read the elements.
///   This causes the filter predicate to be called twice for each
///   element of the underlying collection, which is surprising.
struct FilterCollectionView<Base : CollectionType> : CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType {

    /// A type that represents a valid position in the collection.
    ///
    /// Valid indices consist of the position of every element and a
    /// "past the end" position that's not valid for use as a subscript.
    typealias Index = FilterCollectionViewIndex<Base>

    /// Construct an instance containing the elements of `base` that
    /// satisfy `predicate`.
    init(_ base: Base, includeElement predicate: (Base.Generator.Element) -> Bool)

    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    ///
    /// - Complexity: O(N), where N is the ratio between unfiltered and
    ///   filtered collection counts.
    var startIndex: FilterCollectionViewIndex<Base> { get }

    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    ///
    /// - Complexity: O(1).
    var endIndex: FilterCollectionViewIndex<Base> { get }
    subscript (position: FilterCollectionViewIndex<Base>) -> Base.Generator.Element { get }

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> FilterGenerator<Base.Generator>
}


/// The `Index` used for subscripting a `FilterCollectionView`.
struct FilterCollectionViewIndex<Base : CollectionType> : ForwardIndexType, _Incrementable, Equatable {

    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    func successor() -> FilterCollectionViewIndex<Base>
}


/// The `GeneratorType` used by `FilterSequenceView` and
/// `FilterCollectionView`.
struct FilterGenerator<Base : GeneratorType> : GeneratorType, SequenceType {

    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: `next()` has not been applied to a copy of `self`
    ///   since the copy was made, and no preceding call to `self.next()`
    ///   has returned `nil`.
    mutating func next() -> Base.Element?

    /// `FilterGenerator` is also a `SequenceType`, so it `generate`'s
    /// a copy of itself.
    func generate() -> FilterGenerator<Base>
}


/// The lazy `SequenceType` returned by `filter(c)` where `c` is a
/// `SequenceType`.
struct FilterSequenceView<Base : SequenceType> : SequenceType {

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> FilterGenerator<Base.Generator>
}

struct Float {

    /// Create an instance initialized to zero.
    init()
    init(_bits v: Builtin.FPIEEE32)

    /// Create an instance initialized to `value`.
    init(_ value: Float)
}

extension Float : CustomStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }
}

extension Float : FloatingPointType {

    /// The positive infinity.
    static var infinity: Float { get }

    /// A quiet NaN.
    static var NaN: Float { get }

    /// A quiet NaN.
    static var quietNaN: Float { get }

    /// `true` iff `self` is negative.
    var isSignMinus: Bool { get }

    /// `true` iff `self` is normal (not zero, subnormal, infinity, or
    /// NaN).
    var isNormal: Bool { get }

    /// `true` iff `self` is zero, subnormal, or normal (not infinity
    /// or NaN).
    var isFinite: Bool { get }

    /// `true` iff `self` is +0.0 or -0.0.
    var isZero: Bool { get }

    /// `true` iff `self` is subnormal.
    var isSubnormal: Bool { get }

    /// `true` iff `self` is infinity.
    var isInfinite: Bool { get }

    /// `true` iff `self` is NaN.
    var isNaN: Bool { get }

    /// `true` iff `self` is a signaling NaN.
    var isSignaling: Bool { get }
}

extension Float {

    /// The IEEE 754 "class" of this type.
    var floatingPointClass: FloatingPointClassification { get }
}

extension Float : IntegerLiteralConvertible {
    init(_builtinIntegerLiteral value: Builtin.Int2048)

    /// Create an instance initialized to `value`.
    init(integerLiteral value: Int64)
}

extension Float {
    init(_builtinFloatLiteral value: Builtin.FPIEEE80)
}

extension Float : FloatLiteralConvertible {

    /// Create an instance initialized to `value`.
    init(floatLiteral value: Float)
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
    var hashValue: Int { get }
}

extension Float : SignedNumberType, AbsoluteValuable {

    /// Returns the absolute value of `x`.
    static func abs(x: Float) -> Float
}

extension Float {
    init(_ v: UInt8)
    init(_ v: Int8)
    init(_ v: UInt16)
    init(_ v: Int16)
    init(_ v: UInt32)
    init(_ v: Int32)
    init(_ v: UInt64)
    init(_ v: Int64)
    init(_ v: UInt)
    init(_ v: Int)
}

extension Float {

    /// Construct an instance that approximates `other`.
    init(_ other: Double)

    /// Construct an instance that approximates `other`.
    init(_ other: Float80)
}

extension Float : Strideable, _Strideable {

    /// Returns a stride `x` such that `self.advancedBy(x)` approximates
    /// `other`.
    ///
    /// - Complexity: O(1).
    func distanceTo(other: Float) -> Float

    /// Returns a `Self` `x` such that `self.distanceTo(x)` approximates
    /// `n`.
    ///
    /// - Complexity: O(1).
    func advancedBy(amount: Float) -> Float
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
    init?(_ text: String)
}

extension Float : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension Float : _CVarArgPassedAsDouble, CVarArgType, _CVarArgAlignedType {
}


/// A 32-bit floating point type.
typealias Float32 = Float


/// A 64-bit floating point type.
typealias Float64 = Double

struct Float80 {

    /// Create an instance initialized to zero.
    init()
    init(_bits v: Builtin.FPIEEE80)

    /// Create an instance initialized to `value`.
    init(_ value: Float80)
}

extension Float80 : CustomStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }
}

extension Float80 : IntegerLiteralConvertible {
    init(_builtinIntegerLiteral value: Builtin.Int2048)

    /// Create an instance initialized to `value`.
    init(integerLiteral value: Int64)
}

extension Float80 {
    init(_builtinFloatLiteral value: Builtin.FPIEEE80)
}

extension Float80 : FloatLiteralConvertible {

    /// Create an instance initialized to `value`.
    init(floatLiteral value: Float80)
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
    var hashValue: Int { get }
}

extension Float80 : AbsoluteValuable, SignedNumberType {

    /// Returns the absolute value of `x`.
    static func abs(x: Float80) -> Float80
}

extension Float80 {
    init(_ v: UInt8)
    init(_ v: Int8)
    init(_ v: UInt16)
    init(_ v: Int16)
    init(_ v: UInt32)
    init(_ v: Int32)
    init(_ v: UInt64)
    init(_ v: Int64)
    init(_ v: UInt)
    init(_ v: Int)
}

extension Float80 {

    /// Construct an instance that approximates `other`.
    init(_ other: Float)

    /// Construct an instance that approximates `other`.
    init(_ other: Double)
}

extension Float80 : Strideable, _Strideable {

    /// Returns a stride `x` such that `self.advancedBy(x)` approximates
    /// `other`.
    ///
    /// - Complexity: O(1).
    func distanceTo(other: Float80) -> Float80

    /// Returns a `Self` `x` such that `self.distanceTo(x)` approximates
    /// `n`.
    ///
    /// - Complexity: O(1).
    func advancedBy(amount: Float80) -> Float80
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
    init?(_ text: String)
}


/// Conforming types can be initialized with floating point literals.
protocol FloatLiteralConvertible {
    typealias FloatLiteralType

    /// Create an instance initialized to `value`.
    init(floatLiteral value: Self.FloatLiteralType)
}


/// The default type for an otherwise-unconstrained floating point literal.
typealias FloatLiteralType = Double


/// The set of possible IEEE 754 "classes"
enum FloatingPointClassification {
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
protocol FloatingPointType : Strideable {

    /// Create an instance initialized to `value`.
    init(_ value: UInt8)

    /// Create an instance initialized to `value`.
    init(_ value: Int8)

    /// Create an instance initialized to `value`.
    init(_ value: UInt16)

    /// Create an instance initialized to `value`.
    init(_ value: Int16)

    /// Create an instance initialized to `value`.
    init(_ value: UInt32)

    /// Create an instance initialized to `value`.
    init(_ value: Int32)

    /// Create an instance initialized to `value`.
    init(_ value: UInt64)

    /// Create an instance initialized to `value`.
    init(_ value: Int64)

    /// Create an instance initialized to `value`.
    init(_ value: UInt)

    /// Create an instance initialized to `value`.
    init(_ value: Int)

    /// The positive infinity.
    static var infinity: Self { get }

    /// A quiet NaN.
    static var NaN: Self { get }

    /// A quiet NaN.
    static var quietNaN: Self { get }

    /// The IEEE 754 "class" of this type.
    var floatingPointClass: FloatingPointClassification { get }

    /// `true` iff `self` is negative.
    var isSignMinus: Bool { get }

    /// `true` iff `self` is normal (not zero, subnormal, infinity, or
    /// NaN).
    var isNormal: Bool { get }

    /// `true` iff `self` is zero, subnormal, or normal (not infinity
    /// or NaN).
    var isFinite: Bool { get }

    /// `true` iff `self` is +0.0 or -0.0.
    var isZero: Bool { get }

    /// `true` iff `self` is subnormal.
    var isSubnormal: Bool { get }

    /// `true` iff `self` is infinity.
    var isInfinite: Bool { get }

    /// `true` iff `self` is NaN.
    var isNaN: Bool { get }

    /// `true` iff `self` is a signaling NaN.
    var isSignaling: Bool { get }
}


/// Represents a discrete value in a series, where a value's
/// successor, if any, is reachable by applying the value's
/// `successor()` method.
protocol ForwardIndexType : _Incrementable {

    /// A type that can represent the number of steps between pairs of
    /// `Self` values where one value is reachable from the other.
    ///
    /// Reachability is defined by the ability to produce one value from
    /// the other via zero or more applications of `successor`.
    typealias Distance : _SignedIntegerType = Int
}


/// A generator that produces one or fewer instances of `T`.
struct GeneratorOfOne<T> : GeneratorType, SequenceType {

    /// Construct an instance that generates `element!`, or an empty
    /// sequence if `element == nil`.
    init(_ element: T?)

    /// `GeneratorOfOne` is also a `SequenceType`, so it `generate`s a
    /// copy of itself.
    func generate() -> GeneratorOfOne<T>

    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: `next()` has not been applied to a copy of `self`
    ///   since the copy was made, and no preceding call to `self.next()`
    ///   has returned `nil`.
    mutating func next() -> T?
}


/// A sequence built around a generator of type `G`.
///
/// Useful mostly to recover the ability to use `for`...`in`,
/// given just a generator `g`:
///
///     for x in GeneratorSequence(g) { ... }
struct GeneratorSequence<G : GeneratorType> : GeneratorType, SequenceType {

    /// Construct an instance whose generator is a copy of `base`.
    init(_ base: G)

    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: `next()` has not been applied to a copy of `self`
    ///   since the copy was made, and no preceding call to `self.next()`
    ///   has returned `nil`.
    mutating func next() -> G.Element?

    /// Returns a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> GeneratorSequence<G>
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
protocol GeneratorType {

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
    mutating func next() -> Self.Element?
}


/// A half-open `IntervalType`, which contains its `start` but not its
/// `end`.  Can represent an empty interval.
struct HalfOpenInterval<T : Comparable> : IntervalType, Equatable, CustomStringConvertible, CustomDebugStringConvertible, Reflectable {

    /// The type of the `Interval`'s endpoints.
    typealias Bound = T

    /// Construct a copy of `x`.
    init(_ x: HalfOpenInterval<T>)

    /// Construct an interval with the given bounds.
    ///
    /// - Requires: `start <= end`.
    init(_ start: T, _ end: T)

    /// The `Interval`'s lower bound.
    ///
    /// Invariant: `start` <= `end`.
    var start: T { get }

    /// The `Interval`'s upper bound.
    ///
    /// Invariant: `start` <= `end`.
    var end: T { get }

    /// A textual representation of `self`.
    var description: String { get }

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }

    /// Returns `true` iff the `Interval` contains `x`.
    func contains(x: T) -> Bool

    /// Returns `intervalToClamp` clamped to `self`.
    ///
    /// The bounds of the result, even if it is empty, are always limited to the bounds of
    /// `self`.
    func clamp(intervalToClamp: HalfOpenInterval<T>) -> HalfOpenInterval<T>

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension HalfOpenInterval {

    /// `true` iff the `Interval` is empty.
    var isEmpty: Bool { get }
}


/// Instances of conforming types provide an integer `hashValue` and
/// can be used as `Dictionary` keys.
protocol Hashable : Equatable {

    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    var hashValue: Int { get }
}


/// An optional type that allows implicit member access (via compiler
/// magic).
///
/// The compiler has special knowledge of the existence of
/// `ImplicitlyUnwrappedOptional<T>`, but always interacts with it using the
/// library intrinsics below.
enum ImplicitlyUnwrappedOptional<T> : Reflectable, NilLiteralConvertible {
    case None
    case Some(T)

    /// Construct a `nil` instance.
    init()

    /// Construct a non-`nil` instance that stores `some`.
    init(_ some: T)

    /// Construct an instance from an explicitly unwrapped optional
    /// (`T?`).
    init(_ v: T?)

    /// Create an instance initialized with `nil`.
    init(nilLiteral: ())

    /// If `self == nil`, returns `nil`.  Otherwise, returns `f(self!)`.
    func map<U>(@noescape f: (T) -> U) -> U!

    /// Returns `nil` if `self` is nil, `f(self!)` otherwise.
    func flatMap<U>(@noescape f: (T) -> U!) -> U!

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension ImplicitlyUnwrappedOptional : CustomStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }
}

extension ImplicitlyUnwrappedOptional : _ObjectiveCBridgeable {
}


/// A *generator* for an arbitrary *collection*.  Provided `C`
/// conforms to the other requirements of *CollectionType*,
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
struct IndexingGenerator<C : _CollectionGeneratorDefaultsType> : GeneratorType, SequenceType {

    /// Create a *generator* over the given collection.
    init(_ seq: C)

    /// Returns a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> IndexingGenerator<C>

    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: No preceding call to `self.next()` has returned `nil`.
    mutating func next() -> C._Element?
}


/// A 64-bit signed integer value
/// type.
struct Int : Equatable, _SignedIntegerType, Comparable, _IntegerType, IntegerArithmeticType, _IntegerArithmeticType, SignedIntegerType, IntegerType {
    var value: Builtin.Int64

    /// A type that can represent the number of steps between pairs of
    /// values.
    typealias Distance = Int

    /// Create an instance initialized to zero.
    init()
    init(_ v: Builtin.Word)

    /// Create an instance initialized to `value`.
    init(_ value: Int)

    /// Creates an integer from its big-endian representation, changing the
    /// byte order if necessary.
    init(bigEndian value: Int)

    /// Creates an integer from its little-endian representation, changing the
    /// byte order if necessary.
    init(littleEndian value: Int)
    init(_builtinIntegerLiteral value: Builtin.Int2048)

    /// Create an instance initialized to `value`.
    init(integerLiteral value: Int)

    /// Returns the big-endian representation of the integer, changing the
    /// byte order if necessary.
    var bigEndian: Int { get }

    /// Returns the little-endian representation of the integer, changing the
    /// byte order if necessary.
    var littleEndian: Int { get }

    /// Returns the current integer with the byte order swapped.
    var byteSwapped: Int { get }
    static var max: Int { get }
    static var min: Int { get }
}

extension Int : Hashable {

    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    var hashValue: Int { get }
}

extension Int : CustomStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }
}

extension Int : ForwardIndexType, _Incrementable, RandomAccessIndexType, BidirectionalIndexType, Strideable, _Strideable {

    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    func successor() -> Int

    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    func predecessor() -> Int

    /// Return the minimum number of applications of `successor` or
    /// `predecessor` required to reach `other` from `self`.
    ///
    /// - Complexity: O(1).
    func distanceTo(other: Int) -> Distance

    /// Return `self` offset by `n` steps.
    ///
    /// - Returns: If `n > 0`, the result of applying `successor` to
    ///   `self` `n` times.  If `n < 0`, the result of applying
    ///   `predecessor` to `self` `-n` times. Otherwise, `self`.
    ///
    /// - Complexity: O(1).
    func advancedBy(amount: Distance) -> Int
}

extension Int {

    /// Add `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func addWithOverflow(lhs: Int, _ rhs: Int) -> (Int, overflow: Bool)

    /// Subtract `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func subtractWithOverflow(lhs: Int, _ rhs: Int) -> (Int, overflow: Bool)

    /// Multiply `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func multiplyWithOverflow(lhs: Int, _ rhs: Int) -> (Int, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning
    /// a result and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    static func divideWithOverflow(lhs: Int, _ rhs: Int) -> (Int, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning
    /// the remainder and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    static func remainderWithOverflow(lhs: Int, _ rhs: Int) -> (Int, overflow: Bool)

    /// Represent this number using Swift's widest native signed
    /// integer type.
    func toIntMax() -> IntMax
}

extension Int : SignedNumberType, IntegerLiteralConvertible {
}

extension Int {
    init(_ v: UInt8)
    init(_ v: Int8)
    init(_ v: UInt16)
    init(_ v: Int16)
    init(_ v: UInt32)
    init(_ v: Int32)
    init(_ v: UInt64)

    /// Construct a `Int` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: UInt64)
    init(_ v: Int64)

    /// Construct a `Int` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: Int64)
    init(_ v: UInt)

    /// Construct a `Int` having the same memory representation as
    /// the `UInt` `bitPattern`.  No range or overflow checking
    /// occurs, and the resulting `Int` may not have the same numeric
    /// value as `bitPattern`--it is only guaranteed to use the same
    /// pattern of bits.
    init(bitPattern: UInt)
}

extension Int : BitwiseOperationsType {

    /// The empty bitset of type Int.
    static var allZeros: Int { get }
}

extension Int {

    /// Construct an instance that approximates `other`.
    init(_ other: Float)

    /// Construct an instance that approximates `other`.
    init(_ other: Double)

    /// Construct an instance that approximates `other`.
    init(_ other: Float80)
}

extension Int {

    /// Construct from an ASCII representation in the given `radix`.
    ///
    /// If `text` does not match the regular expression
    /// "[+-][0-9a-zA-Z]+", or the value it denotes in the given `radix`
    /// is not representable, the result is `nil`.
    init?(_ text: String, radix: Int = default)
}

extension Int : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension Int : MirrorPathType {
}

extension Int : CVarArgType {
}


/// A 16-bit signed integer value
/// type.
struct Int16 : SignedIntegerType, IntegerType, Equatable, Comparable, _IntegerType, IntegerArithmeticType, _IntegerArithmeticType, _SignedIntegerType {
    var value: Builtin.Int16

    /// A type that can represent the number of steps between pairs of
    /// values.
    typealias Distance = Int

    /// Create an instance initialized to zero.
    init()

    /// Create an instance initialized to `value`.
    init(_ value: Int16)

    /// Creates an integer from its big-endian representation, changing the
    /// byte order if necessary.
    init(bigEndian value: Int16)

    /// Creates an integer from its little-endian representation, changing the
    /// byte order if necessary.
    init(littleEndian value: Int16)
    init(_builtinIntegerLiteral value: Builtin.Int2048)

    /// Create an instance initialized to `value`.
    init(integerLiteral value: Int16)

    /// Returns the big-endian representation of the integer, changing the
    /// byte order if necessary.
    var bigEndian: Int16 { get }

    /// Returns the little-endian representation of the integer, changing the
    /// byte order if necessary.
    var littleEndian: Int16 { get }

    /// Returns the current integer with the byte order swapped.
    var byteSwapped: Int16 { get }
    static var max: Int16 { get }
    static var min: Int16 { get }
}

extension Int16 : Hashable {

    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    var hashValue: Int { get }
}

extension Int16 : CustomStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }
}

extension Int16 : RandomAccessIndexType, BidirectionalIndexType, ForwardIndexType, _Incrementable, Strideable, _Strideable {

    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    func successor() -> Int16

    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    func predecessor() -> Int16

    /// Return the minimum number of applications of `successor` or
    /// `predecessor` required to reach `other` from `self`.
    ///
    /// - Complexity: O(1).
    func distanceTo(other: Int16) -> Distance

    /// Return `self` offset by `n` steps.
    ///
    /// - Returns: If `n > 0`, the result of applying `successor` to
    ///   `self` `n` times.  If `n < 0`, the result of applying
    ///   `predecessor` to `self` `-n` times. Otherwise, `self`.
    ///
    /// - Complexity: O(1).
    func advancedBy(amount: Distance) -> Int16
}

extension Int16 {

    /// Add `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func addWithOverflow(lhs: Int16, _ rhs: Int16) -> (Int16, overflow: Bool)

    /// Subtract `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func subtractWithOverflow(lhs: Int16, _ rhs: Int16) -> (Int16, overflow: Bool)

    /// Multiply `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func multiplyWithOverflow(lhs: Int16, _ rhs: Int16) -> (Int16, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning
    /// a result and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    static func divideWithOverflow(lhs: Int16, _ rhs: Int16) -> (Int16, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning
    /// the remainder and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    static func remainderWithOverflow(lhs: Int16, _ rhs: Int16) -> (Int16, overflow: Bool)

    /// Represent this number using Swift's widest native signed
    /// integer type.
    func toIntMax() -> IntMax
}

extension Int16 : IntegerLiteralConvertible, SignedNumberType {
}

extension Int16 {
    init(_ v: UInt8)
    init(_ v: Int8)
    init(_ v: UInt16)
    init(_ v: UInt32)

    /// Construct a `Int16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: UInt32)
    init(_ v: Int32)

    /// Construct a `Int16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: Int32)
    init(_ v: UInt64)

    /// Construct a `Int16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: UInt64)
    init(_ v: Int64)

    /// Construct a `Int16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: Int64)
    init(_ v: UInt)

    /// Construct a `Int16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: UInt)
    init(_ v: Int)

    /// Construct a `Int16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: Int)

    /// Construct a `Int16` having the same memory representation as
    /// the `UInt16` `bitPattern`.  No range or overflow checking
    /// occurs, and the resulting `Int16` may not have the same numeric
    /// value as `bitPattern`--it is only guaranteed to use the same
    /// pattern of bits.
    init(bitPattern: UInt16)
}

extension Int16 : BitwiseOperationsType {

    /// The empty bitset of type Int16.
    static var allZeros: Int16 { get }
}

extension Int16 {

    /// Construct an instance that approximates `other`.
    init(_ other: Float)

    /// Construct an instance that approximates `other`.
    init(_ other: Double)

    /// Construct an instance that approximates `other`.
    init(_ other: Float80)
}

extension Int16 {

    /// Construct from an ASCII representation in the given `radix`.
    ///
    /// If `text` does not match the regular expression
    /// "[+-][0-9a-zA-Z]+", or the value it denotes in the given `radix`
    /// is not representable, the result is `nil`.
    init?(_ text: String, radix: Int = default)
}

extension Int16 : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension Int16 : CVarArgType {
}


/// A 32-bit signed integer value
/// type.
struct Int32 : SignedIntegerType, IntegerType, Equatable, Comparable, _IntegerType, IntegerArithmeticType, _IntegerArithmeticType, _SignedIntegerType {
    var value: Builtin.Int32

    /// A type that can represent the number of steps between pairs of
    /// values.
    typealias Distance = Int

    /// Create an instance initialized to zero.
    init()

    /// Create an instance initialized to `value`.
    init(_ value: Int32)

    /// Creates an integer from its big-endian representation, changing the
    /// byte order if necessary.
    init(bigEndian value: Int32)

    /// Creates an integer from its little-endian representation, changing the
    /// byte order if necessary.
    init(littleEndian value: Int32)
    init(_builtinIntegerLiteral value: Builtin.Int2048)

    /// Create an instance initialized to `value`.
    init(integerLiteral value: Int32)

    /// Returns the big-endian representation of the integer, changing the
    /// byte order if necessary.
    var bigEndian: Int32 { get }

    /// Returns the little-endian representation of the integer, changing the
    /// byte order if necessary.
    var littleEndian: Int32 { get }

    /// Returns the current integer with the byte order swapped.
    var byteSwapped: Int32 { get }
    static var max: Int32 { get }
    static var min: Int32 { get }
}

extension Int32 : Hashable {

    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    var hashValue: Int { get }
}

extension Int32 : CustomStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }
}

extension Int32 : RandomAccessIndexType, BidirectionalIndexType, ForwardIndexType, _Incrementable, Strideable, _Strideable {

    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    func successor() -> Int32

    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    func predecessor() -> Int32

    /// Return the minimum number of applications of `successor` or
    /// `predecessor` required to reach `other` from `self`.
    ///
    /// - Complexity: O(1).
    func distanceTo(other: Int32) -> Distance

    /// Return `self` offset by `n` steps.
    ///
    /// - Returns: If `n > 0`, the result of applying `successor` to
    ///   `self` `n` times.  If `n < 0`, the result of applying
    ///   `predecessor` to `self` `-n` times. Otherwise, `self`.
    ///
    /// - Complexity: O(1).
    func advancedBy(amount: Distance) -> Int32
}

extension Int32 {

    /// Add `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func addWithOverflow(lhs: Int32, _ rhs: Int32) -> (Int32, overflow: Bool)

    /// Subtract `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func subtractWithOverflow(lhs: Int32, _ rhs: Int32) -> (Int32, overflow: Bool)

    /// Multiply `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func multiplyWithOverflow(lhs: Int32, _ rhs: Int32) -> (Int32, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning
    /// a result and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    static func divideWithOverflow(lhs: Int32, _ rhs: Int32) -> (Int32, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning
    /// the remainder and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    static func remainderWithOverflow(lhs: Int32, _ rhs: Int32) -> (Int32, overflow: Bool)

    /// Represent this number using Swift's widest native signed
    /// integer type.
    func toIntMax() -> IntMax
}

extension Int32 : IntegerLiteralConvertible, SignedNumberType {
}

extension Int32 {
    init(_ v: UInt8)
    init(_ v: Int8)
    init(_ v: UInt16)
    init(_ v: Int16)
    init(_ v: UInt32)
    init(_ v: UInt64)

    /// Construct a `Int32` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: UInt64)
    init(_ v: Int64)

    /// Construct a `Int32` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: Int64)
    init(_ v: UInt)

    /// Construct a `Int32` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: UInt)
    init(_ v: Int)

    /// Construct a `Int32` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: Int)

    /// Construct a `Int32` having the same memory representation as
    /// the `UInt32` `bitPattern`.  No range or overflow checking
    /// occurs, and the resulting `Int32` may not have the same numeric
    /// value as `bitPattern`--it is only guaranteed to use the same
    /// pattern of bits.
    init(bitPattern: UInt32)
}

extension Int32 : BitwiseOperationsType {

    /// The empty bitset of type Int32.
    static var allZeros: Int32 { get }
}

extension Int32 {

    /// Construct an instance that approximates `other`.
    init(_ other: Float)

    /// Construct an instance that approximates `other`.
    init(_ other: Double)

    /// Construct an instance that approximates `other`.
    init(_ other: Float80)
}

extension Int32 {

    /// Construct from an ASCII representation in the given `radix`.
    ///
    /// If `text` does not match the regular expression
    /// "[+-][0-9a-zA-Z]+", or the value it denotes in the given `radix`
    /// is not representable, the result is `nil`.
    init?(_ text: String, radix: Int = default)
}

extension Int32 : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension Int32 : CVarArgType {
}


/// A 64-bit signed integer value
/// type.
struct Int64 : _SignedIntegerType, Comparable, Equatable, _IntegerType, IntegerArithmeticType, _IntegerArithmeticType, SignedIntegerType, IntegerType {
    var value: Builtin.Int64

    /// A type that can represent the number of steps between pairs of
    /// values.
    typealias Distance = Int

    /// Create an instance initialized to zero.
    init()

    /// Create an instance initialized to `value`.
    init(_ value: Int64)

    /// Creates an integer from its big-endian representation, changing the
    /// byte order if necessary.
    init(bigEndian value: Int64)

    /// Creates an integer from its little-endian representation, changing the
    /// byte order if necessary.
    init(littleEndian value: Int64)
    init(_builtinIntegerLiteral value: Builtin.Int2048)

    /// Create an instance initialized to `value`.
    init(integerLiteral value: Int64)

    /// Returns the big-endian representation of the integer, changing the
    /// byte order if necessary.
    var bigEndian: Int64 { get }

    /// Returns the little-endian representation of the integer, changing the
    /// byte order if necessary.
    var littleEndian: Int64 { get }

    /// Returns the current integer with the byte order swapped.
    var byteSwapped: Int64 { get }
    static var max: Int64 { get }
    static var min: Int64 { get }
}

extension Int64 : Hashable {

    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    var hashValue: Int { get }
}

extension Int64 : CustomStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }
}

extension Int64 : _Incrementable, RandomAccessIndexType, BidirectionalIndexType, ForwardIndexType, Strideable, _Strideable {

    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    func successor() -> Int64

    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    func predecessor() -> Int64

    /// Return the minimum number of applications of `successor` or
    /// `predecessor` required to reach `other` from `self`.
    ///
    /// - Complexity: O(1).
    func distanceTo(other: Int64) -> Distance

    /// Return `self` offset by `n` steps.
    ///
    /// - Returns: If `n > 0`, the result of applying `successor` to
    ///   `self` `n` times.  If `n < 0`, the result of applying
    ///   `predecessor` to `self` `-n` times. Otherwise, `self`.
    ///
    /// - Complexity: O(1).
    func advancedBy(amount: Distance) -> Int64
}

extension Int64 {

    /// Add `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func addWithOverflow(lhs: Int64, _ rhs: Int64) -> (Int64, overflow: Bool)

    /// Subtract `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func subtractWithOverflow(lhs: Int64, _ rhs: Int64) -> (Int64, overflow: Bool)

    /// Multiply `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func multiplyWithOverflow(lhs: Int64, _ rhs: Int64) -> (Int64, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning
    /// a result and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    static func divideWithOverflow(lhs: Int64, _ rhs: Int64) -> (Int64, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning
    /// the remainder and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    static func remainderWithOverflow(lhs: Int64, _ rhs: Int64) -> (Int64, overflow: Bool)

    /// Represent this number using Swift's widest native signed
    /// integer type.
    func toIntMax() -> IntMax
}

extension Int64 : SignedNumberType, IntegerLiteralConvertible {
}

extension Int64 {
    init(_ v: UInt8)
    init(_ v: Int8)
    init(_ v: UInt16)
    init(_ v: Int16)
    init(_ v: UInt32)
    init(_ v: Int32)
    init(_ v: UInt64)
    init(_ v: UInt)
    init(_ v: Int)

    /// Construct a `Int64` having the same memory representation as
    /// the `UInt64` `bitPattern`.  No range or overflow checking
    /// occurs, and the resulting `Int64` may not have the same numeric
    /// value as `bitPattern`--it is only guaranteed to use the same
    /// pattern of bits.
    init(bitPattern: UInt64)
}

extension Int64 : BitwiseOperationsType {

    /// The empty bitset of type Int64.
    static var allZeros: Int64 { get }
}

extension Int64 {

    /// Construct an instance that approximates `other`.
    init(_ other: Float)

    /// Construct an instance that approximates `other`.
    init(_ other: Double)

    /// Construct an instance that approximates `other`.
    init(_ other: Float80)
}

extension Int64 {

    /// Construct from an ASCII representation in the given `radix`.
    ///
    /// If `text` does not match the regular expression
    /// "[+-][0-9a-zA-Z]+", or the value it denotes in the given `radix`
    /// is not representable, the result is `nil`.
    init?(_ text: String, radix: Int = default)
}

extension Int64 : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension Int64 : MirrorPathType {
}

extension Int64 : CVarArgType, _CVarArgAlignedType {
}


/// A 8-bit signed integer value
/// type.
struct Int8 : SignedIntegerType, IntegerType, Equatable, Comparable, _IntegerType, IntegerArithmeticType, _IntegerArithmeticType, _SignedIntegerType {
    var value: Builtin.Int8

    /// A type that can represent the number of steps between pairs of
    /// values.
    typealias Distance = Int

    /// Create an instance initialized to zero.
    init()

    /// Create an instance initialized to `value`.
    init(_ value: Int8)
    init(_builtinIntegerLiteral value: Builtin.Int2048)

    /// Create an instance initialized to `value`.
    init(integerLiteral value: Int8)
    static var max: Int8 { get }
    static var min: Int8 { get }
}

extension Int8 : Hashable {

    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    var hashValue: Int { get }
}

extension Int8 : CustomStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }
}

extension Int8 : RandomAccessIndexType, BidirectionalIndexType, ForwardIndexType, _Incrementable, Strideable, _Strideable {

    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    func successor() -> Int8

    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    func predecessor() -> Int8

    /// Return the minimum number of applications of `successor` or
    /// `predecessor` required to reach `other` from `self`.
    ///
    /// - Complexity: O(1).
    func distanceTo(other: Int8) -> Distance

    /// Return `self` offset by `n` steps.
    ///
    /// - Returns: If `n > 0`, the result of applying `successor` to
    ///   `self` `n` times.  If `n < 0`, the result of applying
    ///   `predecessor` to `self` `-n` times. Otherwise, `self`.
    ///
    /// - Complexity: O(1).
    func advancedBy(amount: Distance) -> Int8
}

extension Int8 {

    /// Add `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func addWithOverflow(lhs: Int8, _ rhs: Int8) -> (Int8, overflow: Bool)

    /// Subtract `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func subtractWithOverflow(lhs: Int8, _ rhs: Int8) -> (Int8, overflow: Bool)

    /// Multiply `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func multiplyWithOverflow(lhs: Int8, _ rhs: Int8) -> (Int8, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning
    /// a result and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    static func divideWithOverflow(lhs: Int8, _ rhs: Int8) -> (Int8, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning
    /// the remainder and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    static func remainderWithOverflow(lhs: Int8, _ rhs: Int8) -> (Int8, overflow: Bool)

    /// Represent this number using Swift's widest native signed
    /// integer type.
    func toIntMax() -> IntMax
}

extension Int8 : IntegerLiteralConvertible, SignedNumberType {
}

extension Int8 {
    init(_ v: UInt8)
    init(_ v: UInt16)

    /// Construct a `Int8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: UInt16)
    init(_ v: Int16)

    /// Construct a `Int8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: Int16)
    init(_ v: UInt32)

    /// Construct a `Int8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: UInt32)
    init(_ v: Int32)

    /// Construct a `Int8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: Int32)
    init(_ v: UInt64)

    /// Construct a `Int8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: UInt64)
    init(_ v: Int64)

    /// Construct a `Int8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: Int64)
    init(_ v: UInt)

    /// Construct a `Int8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: UInt)
    init(_ v: Int)

    /// Construct a `Int8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: Int)

    /// Construct a `Int8` having the same memory representation as
    /// the `UInt8` `bitPattern`.  No range or overflow checking
    /// occurs, and the resulting `Int8` may not have the same numeric
    /// value as `bitPattern`--it is only guaranteed to use the same
    /// pattern of bits.
    init(bitPattern: UInt8)
}

extension Int8 : BitwiseOperationsType {

    /// The empty bitset of type Int8.
    static var allZeros: Int8 { get }
}

extension Int8 {

    /// Construct an instance that approximates `other`.
    init(_ other: Float)

    /// Construct an instance that approximates `other`.
    init(_ other: Double)

    /// Construct an instance that approximates `other`.
    init(_ other: Float80)
}

extension Int8 {

    /// Construct from an ASCII representation in the given `radix`.
    ///
    /// If `text` does not match the regular expression
    /// "[+-][0-9a-zA-Z]+", or the value it denotes in the given `radix`
    /// is not representable, the result is `nil`.
    init?(_ text: String, radix: Int = default)
}

extension Int8 : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension Int8 : CVarArgType {
}


/// The largest native signed integer type.
typealias IntMax = Int64


/// The common requirements for types that support integer arithmetic.
protocol IntegerArithmeticType : _IntegerArithmeticType, Comparable {

    /// Add `lhs` and `rhs`, returning a result and trapping in case of
    /// arithmetic overflow (except in -Ounchecked builds).
    func +(lhs: Self, rhs: Self) -> Self

    /// Subtract `lhs` and `rhs`, returning a result and trapping in case of
    /// arithmetic overflow (except in -Ounchecked builds).
    func -(lhs: Self, rhs: Self) -> Self

    /// Multiply `lhs` and `rhs`, returning a result and trapping in case of
    /// arithmetic overflow (except in -Ounchecked builds).
    func *(lhs: Self, rhs: Self) -> Self

    /// Divide `lhs` and `rhs`, returning a result and trapping in case of
    /// arithmetic overflow (except in -Ounchecked builds).
    func /(lhs: Self, rhs: Self) -> Self

    /// Divide `lhs` and `rhs`, returning the remainder and trapping in case of
    /// arithmetic overflow (except in -Ounchecked builds).
    func %(lhs: Self, rhs: Self) -> Self

    /// Explicitly convert to `IntMax`, trapping on overflow (except in
    /// -Ounchecked builds).
    func toIntMax() -> IntMax
}


/// Conforming types can be initialized with integer literals.
protocol IntegerLiteralConvertible {
    typealias IntegerLiteralType

    /// Create an instance initialized to `value`.
    init(integerLiteral value: Self.IntegerLiteralType)
}


/// The default type for an otherwise-unconstrained integer literal.
typealias IntegerLiteralType = Int


/// A set of common requirements for Swift's integer types.
protocol IntegerType : _IntegerType, RandomAccessIndexType {
}


/// An interval over a `Comparable` type.
protocol IntervalType {

    /// The type of the `Interval`'s endpoints.
    typealias Bound : Comparable

    /// Returns `true` iff the interval contains `value`.
    func contains(value: Self.Bound) -> Bool

    /// Return `rhs` clamped to `self`.  The bounds of the result, even
    /// if it is empty, are always within the bounds of `self`.
    func clamp(intervalToClamp: Self) -> Self

    /// `true` iff `self` is empty.
    var isEmpty: Bool { get }

    /// The `Interval`'s lower bound.
    ///
    /// Invariant: `start` <= `end`.
    var start: Self.Bound { get }

    /// The `Interval`'s upper bound.
    ///
    /// Invariant: `start` <= `end`.
    var end: Self.Bound { get }
}


/// A collection that forwards its implementation to an underlying
/// collection instance while exposing lazy computations as methods.
struct LazyBidirectionalCollection<S : CollectionType where S.Index : BidirectionalIndexType> : CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType {

    /// Construct an instance with `base` as its underlying collection
    /// instance.
    init(_ base: S)

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> S.Generator

    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    var startIndex: S.Index { get }

    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    var endIndex: S.Index { get }
    subscript (position: S.Index) -> S.Generator.Element { get }

    /// an Array, created on-demand, containing the elements of this
    /// lazy CollectionType.
    var array: [S.Generator.Element] { get }
    func underestimateCount() -> Int
}

extension LazyBidirectionalCollection {

    /// Return a lazy SequenceType containing the elements `x` of `source` for
    /// which `includeElement(x)` is `true`.
    func filter(includeElement: (S.Generator.Element) -> Bool) -> LazySequence<FilterSequenceView<S>>
}

extension LazyBidirectionalCollection {

    /// Return a `MapCollectionView` over this `LazyBidirectionalCollection`.  The elements of
    /// the result are computed lazily, each time they are read, by
    /// calling `transform` function on a base element.
    func map<U>(transform: (S.Generator.Element) -> U) -> LazyBidirectionalCollection<MapCollectionView<S, U>>
}

extension LazyBidirectionalCollection {

    /// Return a `BidirectionalReverseView` over this `LazyBidirectionalCollection`.  The elements of
    /// the result are computed lazily, each time they are read, by
    /// calling `transform` function on a base element.
    func reverse() -> LazyBidirectionalCollection<BidirectionalReverseView<S>>
}


/// A collection that forwards its implementation to an underlying
/// collection instance while exposing lazy computations as methods.
struct LazyForwardCollection<S : CollectionType where S.Index : ForwardIndexType> : CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType {

    /// Construct an instance with `base` as its underlying collection
    /// instance.
    init(_ base: S)

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> S.Generator

    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    var startIndex: S.Index { get }

    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    var endIndex: S.Index { get }
    subscript (position: S.Index) -> S.Generator.Element { get }

    /// an Array, created on-demand, containing the elements of this
    /// lazy CollectionType.
    var array: [S.Generator.Element] { get }
    func underestimateCount() -> Int
}

extension LazyForwardCollection {

    /// Return a lazy SequenceType containing the elements `x` of `source` for
    /// which `includeElement(x)` is `true`.
    func filter(includeElement: (S.Generator.Element) -> Bool) -> LazySequence<FilterSequenceView<S>>
}

extension LazyForwardCollection {

    /// Return a `MapCollectionView` over this `LazyForwardCollection`.  The elements of
    /// the result are computed lazily, each time they are read, by
    /// calling `transform` function on a base element.
    func map<U>(transform: (S.Generator.Element) -> U) -> LazyForwardCollection<MapCollectionView<S, U>>
}


/// A collection that forwards its implementation to an underlying
/// collection instance while exposing lazy computations as methods.
struct LazyRandomAccessCollection<S : CollectionType where S.Index : RandomAccessIndexType> : CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType {

    /// Construct an instance with `base` as its underlying collection
    /// instance.
    init(_ base: S)

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> S.Generator

    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    var startIndex: S.Index { get }

    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    var endIndex: S.Index { get }
    subscript (position: S.Index) -> S.Generator.Element { get }

    /// an Array, created on-demand, containing the elements of this
    /// lazy CollectionType.
    var array: [S.Generator.Element] { get }
    func underestimateCount() -> Int
}

extension LazyRandomAccessCollection {

    /// Return a lazy SequenceType containing the elements `x` of `source` for
    /// which `includeElement(x)` is `true`.
    func filter(includeElement: (S.Generator.Element) -> Bool) -> LazySequence<FilterSequenceView<S>>
}

extension LazyRandomAccessCollection {

    /// Return a `MapCollectionView` over this `LazyRandomAccessCollection`.  The elements of
    /// the result are computed lazily, each time they are read, by
    /// calling `transform` function on a base element.
    func map<U>(transform: (S.Generator.Element) -> U) -> LazyRandomAccessCollection<MapCollectionView<S, U>>
}

extension LazyRandomAccessCollection {

    /// Return a `RandomAccessReverseView` over this `LazyRandomAccessCollection`.  The elements of
    /// the result are computed lazily, each time they are read, by
    /// calling `transform` function on a base element.
    func reverse() -> LazyBidirectionalCollection<RandomAccessReverseView<S>>
}


/// A sequence that forwards its implementation to an underlying
/// sequence instance while exposing lazy computations as methods.
struct LazySequence<S : SequenceType> : SequenceType {

    /// Construct an instance with `base` as its underlying sequence
    /// instance.
    init(_ base: S)

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> S.Generator

    /// an Array, created on-demand, containing the elements of this
    /// lazy SequenceType.
    var array: [S.Generator.Element] { get }
    func underestimateCount() -> Int
}

extension LazySequence {

    /// Return a lazy SequenceType containing the elements `x` of `source` for
    /// which `includeElement(x)` is `true`.
    func filter(includeElement: (S.Generator.Element) -> Bool) -> LazySequence<FilterSequenceView<S>>
}

extension LazySequence {

    /// Return a `MapSequenceView` over this `LazySequence`.  The elements of
    /// the result are computed lazily, each time they are read, by
    /// calling `transform` function on a base element.
    func map<U>(transform: (S.Generator.Element) -> U) -> LazySequence<MapSequenceView<S, U>>
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
class ManagedBuffer<Value, Element> : ManagedProtoBuffer<Value, Element> {

    /// Create a new instance of the most-derived class, calling
    /// `initializeValue` on the partially-constructed object to
    /// generate an initial `Value`.
    final class func create(minimumCapacity: Int, initialValue: (ManagedProtoBuffer<Value, Element>) -> Value) -> ManagedBuffer<Value, Element>

    /// The stored `Value` instance.
    final var value: Value
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
struct ManagedBufferPointer<Value, Element> : Equatable {

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
    init(bufferClass: AnyClass, minimumCapacity: Int, initialValue: (buffer: AnyObject, allocatedCount: (AnyObject) -> Int) -> Value)

    /// Manage the given `buffer`.
    ///
    /// - Requires: `buffer` is an instance of a non-`@objc` class whose
    ///   `deinit` destroys its stored `Value` and any constructed
    ///   `Element`s.
    init(unsafeBufferObject buffer: AnyObject)

    /// The stored `Value` instance.
    var value: Value

    /// Return the object instance being used for storage.
    var buffer: AnyObject { get }

    /// The actual number of elements that can be stored in this object.
    ///
    /// This value may be nontrivial to compute; it is usually a good
    /// idea to store this information in the "value" area when
    /// an instance is created.
    var allocatedElementCount: Int { get }

    /// Call `body` with an `UnsafeMutablePointer` to the stored
    /// `Value`.
    ///
    /// - Note: This pointer is only valid
    ///   for the duration of the call to `body`.
    func withUnsafeMutablePointerToValue<R>(body: (UnsafeMutablePointer<Value>) -> R) -> R

    /// Call `body` with an `UnsafeMutablePointer` to the `Element`
    /// storage.
    ///
    /// - Note: This pointer is only valid for the duration of the
    ///   call to `body`.
    func withUnsafeMutablePointerToElements<R>(body: (UnsafeMutablePointer<Element>) -> R) -> R

    /// Call `body` with `UnsafeMutablePointer`s to the stored `Value`
    /// and raw `Element` storage.
    ///
    /// - Note: These pointers are only valid for the duration of the
    ///   call to `body`.
    func withUnsafeMutablePointers<R>(body: (UnsafeMutablePointer<Value>, UnsafeMutablePointer<Element>) -> R) -> R

    /// Returns true iff `self` holds the only strong reference to its buffer.
    ///
    /// See `isUniquelyReferenced` for details.
    mutating func holdsUniqueReference() -> Bool

    /// Returns true iff either `self` holds the only strong reference
    /// to its buffer or the pinned has been 'pinned'.
    ///
    /// See `isUniquelyReferenced` for details.
    mutating func holdsUniqueOrPinnedReference() -> Bool
}


/// A base class of `ManagedBuffer<Value,Element>`, used during
/// instance creation.
///
/// During instance creation, in particular during
/// `ManagedBuffer.create`'s call to initialize, `ManagedBuffer`'s
/// `value` property is as-yet uninitialized, and therefore
/// `ManagedProtoBuffer` does not offer access to the as-yet
/// uninitialized `value` property of `ManagedBuffer`.
class ManagedProtoBuffer<Value, Element> : NonObjectiveCBase {

    /// The actual number of elements that can be stored in this object.
    ///
    /// This value may be nontrivial to compute; it is usually a good
    /// idea to store this information in the "value" area when
    /// an instance is created.
    final var allocatedElementCount: Int { get }

    /// Call `body` with an `UnsafeMutablePointer` to the stored
    /// `Value`.
    ///
    /// - Note: This pointer is only valid for the duration of the
    ///   call to `body`.
    final func withUnsafeMutablePointerToValue<R>(body: (UnsafeMutablePointer<Value>) -> R) -> R

    /// Call `body` with an `UnsafeMutablePointer` to the `Element`
    /// storage.
    ///
    /// - Note: This pointer is only valid for the duration of the
    ///   call to `body`.
    final func withUnsafeMutablePointerToElements<R>(body: (UnsafeMutablePointer<Element>) -> R) -> R

    /// Call `body` with `UnsafeMutablePointer`s to the stored `Value`
    /// and raw `Element` storage.
    ///
    /// - Note: These pointers are only valid for the duration of the
    ///   call to `body`.
    final func withUnsafeMutablePointers<R>(body: (UnsafeMutablePointer<Value>, UnsafeMutablePointer<Element>) -> R) -> R
}


/// A `CollectionType` whose elements consist of those in a `Base`
/// `CollectionType` passed through a transform function returning `T`.
/// These elements are computed lazily, each time they're read, by
/// calling the transform function on a base element.
struct MapCollectionView<Base : CollectionType, T> : CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType {

    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    var startIndex: Base.Index { get }

    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    var endIndex: Base.Index { get }
    subscript (position: Base.Index) -> T { get }

    /// Returns a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> MapSequenceGenerator<Base.Generator, T>
    func underestimateCount() -> Int
}


/// The `GeneratorType` used by `MapSequenceView` and `MapCollectionView`.
/// Produces each element by passing the output of the `Base`
/// `GeneratorType` through a transform function returning `T`.
struct MapSequenceGenerator<Base : GeneratorType, T> : GeneratorType, SequenceType {

    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: `next()` has not been applied to a copy of `self`
    ///   since the copy was made, and no preceding call to `self.next()`
    ///   has returned `nil`.
    mutating func next() -> T?

    /// `MapSequenceGenerator` is also a `SequenceType`, so it
    /// `generate`'s a copy of itself.
    func generate() -> MapSequenceGenerator<Base, T>
}


/// A `SequenceType` whose elements consist of those in a `Base`
/// `SequenceType` passed through a transform function returning `T`.
/// These elements are computed lazily, each time they're read, by
/// calling the transform function on a base element.
struct MapSequenceView<Base : SequenceType, T> : SequenceType {

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> MapSequenceGenerator<Base.Generator, T>
    func underestimateCount() -> Int
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
struct Mirror {

    /// Representation of ancestor classes.
    ///
    /// A `CustomReflectable` class can control how its mirror will
    /// represent ancestor classes by initializing the mirror with a
    /// `AncestorRepresentation`.  This setting has no effect on mirrors
    /// reflecting value type instances.
    enum AncestorRepresentation {
        case Generated
        case Customized(() -> Mirror)
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
    init(reflecting subject: Any)

    /// An element of the reflected instance's structure.  The optional
    /// `label` may be used when appropriate, e.g. to represent the name
    /// of a stored property or of an active `enum` case, and will be
    /// used for lookup when `String`s are passed to the `descendant`
    /// method.
    typealias Child = (label: String?, value: Any)

    /// The type used to represent sub-structure.
    ///
    /// Depending on your needs, you may find it useful to "upgrade"
    /// instances of this type to `AnyBidirectionalCollection` or
    /// `AnyRandomAccessCollection`.  For example, to display the last
    /// 20 children of a mirror if they can be accessed efficiently, you
    /// might write:
    ///
    ///     if let b = AnyBidirectionalCollection(someMirror.children) {
    ///       for i in advance(b.endIndex, -20, b.startIndex)..<b.endIndex {
    ///          print(b[i])
    ///       }
    ///     }
    typealias Children = AnyForwardCollection<Child>

    /// A suggestion of how a `Mirror`'s is to be interpreted.
    ///
    /// Playgrounds and the debugger will show a representation similar
    /// to the one used for instances of the kind indicated by the
    /// `DisplayStyle` case name when the `Mirror` is used for display.
    enum DisplayStyle {
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
    init<T, C : CollectionType where C.Generator.Element == Child>(_ subject: T, children: C, displayStyle: Mirror.DisplayStyle? = default, ancestorRepresentation: Mirror.AncestorRepresentation = default)

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
    init<T, C : CollectionType>(_ subject: T, unlabeledChildren: C, displayStyle: Mirror.DisplayStyle? = default, ancestorRepresentation: Mirror.AncestorRepresentation = default)

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
    init<T>(_ subject: T, children: DictionaryLiteral<String, Any>, displayStyle: Mirror.DisplayStyle? = default, ancestorRepresentation: Mirror.AncestorRepresentation = default)

    /// The static type of the subject being reflected.
    ///
    /// This type may differ from the subject's dynamic type when `self`
    /// is the `superclassMirror()` of another mirror.
    let subjectType: Any.Type

    /// A collection of `Child` elements describing the structure of the
    /// reflected subject.
    let children: Children

    /// Suggests a display style for the reflected subject.
    let displayStyle: Mirror.DisplayStyle?
    func superclassMirror() -> Mirror?
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
    ///     let p0 = advance(children.startIndex, 1, children.endIndex)
    ///     if p0 != children.endIndex {
    ///       let grandChildren = reflect(children[p0].value).children
    ///       SeekTwo: for g in grandChildren {
    ///         if g.label == "two" {
    ///           let greatGrandChildren = reflect(g.value).children
    ///           let p1 = advance(
    ///             greatGrandChildren.startIndex, 3, 
    ///             greatGrandChildren.endIndex)
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
    func descendant(first: MirrorPathType, _ rest: MirrorPathType...) -> Any?
}



extension Mirror : CustomStringConvertible {
    var description: String { get }
}

extension Mirror : CustomReflectable {
    func customMirror() -> Mirror
}




/// How children of this value should be presented in the IDE.
enum MirrorDisposition {
    case Struct
    case Class
    case Enum
    case Tuple
    case Aggregate
    case IndexContainer
    case KeyContainer
    case MembershipContainer
    case Container
    case Optional
    case ObjCObject
}


/// A protocol for legitimate arguments to `Mirror`'s `descendant`
/// method.
///
/// Do not declare new conformances to this protocol; they will not
/// work as expected.
protocol MirrorPathType {
}


/// The type returned by `reflect(x)`; supplies an API for runtime
/// reflection on `x`.
protocol MirrorType {

    /// The instance being reflected.
    var value: Any { get }

    /// Identical to `value.dynamicType`.
    var valueType: Any.Type { get }

    /// A unique identifier for `value` if it is a class instance; `nil`
    /// otherwise.
    var objectIdentifier: ObjectIdentifier? { get }

    /// The count of `value`'s logical children.
    var count: Int { get }
    subscript (i: Int) -> (String, MirrorType) { get }

    /// A string description of `value`.
    var summary: String { get }

    /// A rich representation of `value` for an IDE, or `nil` if none is supplied.
    var quickLookObject: QuickLookObject? { get }

    /// How `value` should be presented in an IDE.
    var disposition: MirrorDisposition { get }
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
protocol MutableCollectionType : CollectionType {
    subscript (position: Self.Index) -> Self.Generator.Element { get set }
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
    mutating func partition(range: Range<Self.Index>, isOrderedBefore: (Self.Generator.Element, Self.Generator.Element) -> Bool) -> Self.Index
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
    mutating func partition(range: Range<Self.Index>) -> Self.Index
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
    func sort() -> [Self.Generator.Element]
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
    func sort(isOrderedBefore: (Self.Generator.Element, Self.Generator.Element) -> Bool) -> [Self.Generator.Element]
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
    mutating func sortInPlace()
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
    mutating func sortInPlace(isOrderedBefore: (Self.Generator.Element, Self.Generator.Element) -> Bool)
}


/// A *collection* with mutable slices.
///
/// For example,
///
///      x[i..<j] = someExpression
///      x[i..<j].mutatingMethod()
protocol MutableSliceable : Sliceable, MutableCollectionType {
    subscript (_: Range<Self.Index>) -> Self.SubSlice { get set }
}


/// Conforming types can be initialized with `nil`.
protocol NilLiteralConvertible {

    /// Create an instance initialized with `nil`.
    init(nilLiteral: ())
}


/// A common base class for classes that need to be non-`@objc`,
/// recognizably in the type system.
///
/// - SeeAlso: `isUniquelyReferenced`
class NonObjectiveCBase {
}


/// A unique identifier for a class instance or metatype. This can be used by
/// reflection clients to recognize cycles in the object graph.
///
/// In Swift, only class instances and metatypes have unique identities. There
/// is no notion of identity for structs, enums, functions, or tuples.
struct ObjectIdentifier : Hashable, Equatable, Comparable {

    /// Convert to a `UInt` that captures the full value of `self`.
    ///
    /// Axiom: `a.uintValue == b.uintValue` iff `a == b`.
    var uintValue: UInt { get }

    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    var hashValue: Int { get }

    /// Construct an instance that uniquely identifies the class instance `x`.
    init(_ x: AnyObject)

    /// Construct an instance that uniquely identifies the metatype `x`.
    init(_ x: Any.Type)
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
protocol OptionSetType : SetAlgebraType, RawRepresentable {

    /// An `OptionSet`'s `Element` type is normally `Self`.
    typealias Element = Self

    /// Convert from a value of `RawValue`, succeeding unconditionally.
    init(rawValue: Self.RawValue)
}

extension OptionSetType {

    /// Returns the set of elements contained in `self`, in `other`, or in
    /// both `self` and `other`.
    func union(other: Self) -> Self

    /// Returns the set of elements contained in both `self` and `other`.
    func intersect(other: Self) -> Self

    /// Returns the set of elements contained in `self` or in `other`,
    /// but not in both `self` and `other`.
    func exclusiveOr(other: Self) -> Self
}

extension OptionSetType where Element == Self {

    /// Returns `true` if `self` contains `member`.
    ///
    /// - Equivalent to `self.intersect([member]) == [member]`
    func contains(member: Self) -> Bool

    /// If `member` is not already contained in `self`, insert it.
    ///
    /// - Equivalent to `self.unionInPlace([member])`
    /// - Postcondition: `self.contains(member)`
    mutating func insert(member: Self)

    /// If `member` is contained in `self`, remove and return it.
    /// Otherwise, return `nil`.
    ///
    /// - Postcondition: `self.intersect([member]).isEmpty`
    mutating func remove(member: Self) -> Self?
}

extension OptionSetType where RawValue : BitwiseOperationsType {

    /// Create an empty instance.
    ///
    /// - Equivalent to `[] as Self`
    convenience init()

    /// Insert all elements of `other` into `self`.
    ///
    /// - Equivalent to replacing `self` with `self.union(other)`.
    /// - Postcondition: `self.isSupersetOf(other)`
    mutating func unionInPlace(other: Self)

    /// Remove all elements of `self` that are not also present in
    /// `other`.
    ///
    /// - Equivalent to replacing `self` with `self.intersect(other)`
    /// - Postcondition: `self.isSubsetOf(other)`
    mutating func intersectInPlace(other: Self)

    /// Replace `self` with a set containing all elements contained in
    /// either `self` or `other`, but not both.
    ///
    /// - Equivalent to replacing `self` with `self.exclusiveOr(other)`
    mutating func exclusiveOrInPlace(other: Self)
}

enum Optional<T> : Reflectable, NilLiteralConvertible {
    case None
    case Some(T)

    /// Construct a `nil` instance.
    init()

    /// Construct a non-`nil` instance that stores `some`.
    init(_ some: T)

    /// If `self == nil`, returns `nil`.  Otherwise, returns `f(self!)`.
    func map<U>(@noescape f: (T) -> U) -> U?

    /// Returns `nil` if `self` is nil, `f(self!)` otherwise.
    func flatMap<U>(@noescape f: (T) -> U?) -> U?

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType

    /// Create an instance initialized with `nil`.
    init(nilLiteral: ())
}

extension Optional : CustomDebugStringConvertible {

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }
}


/// A target of text streaming operations.
protocol OutputStreamType {

    /// Append the given `string` to this stream.
    mutating func write(string: String)
}




/// A *generator* that adapts a *collection* `C` and any *sequence* of
/// its `Index` type to present the collection's elements in a
/// permuted order.
struct PermutationGenerator<C : CollectionType, Indices : SequenceType where C.Index == Indices.Generator.Element> : GeneratorType, SequenceType {

    /// The type of element returned by `next()`.
    typealias Element = C.Generator.Element

    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: No preceding call to `self.next()` has returned `nil`.
    mutating func next() -> C.Generator.Element?

    /// A type whose instances can produce the elements of this
    /// sequence, in order.
    typealias Generator = PermutationGenerator<C, Indices>

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> PermutationGenerator<C, Indices>

    /// Construct a *generator* over a permutation of `elements` given
    /// by `indices`.
    ///
    /// - Requires: `elements[i]` is valid for every `i` in `indices`.
    init(elements: C, indices: Indices)
}

typealias PlaygroundQuickLook = QuickLookObject

enum Process {

    /// The list of command-line arguments with which the current
    /// process was invoked.
    static let arguments: [String]

    /// Access to the raw argc value from C.
    static var argc: CInt { get }

    /// Access to the raw argv value from C. Accessing the argument vector
    /// through this pointer is unsafe.
    static var unsafeArgv: UnsafeMutablePointer<UnsafeMutablePointer<Int8>> { get }
}


/// The sum of types that can be used as a quick look representation.
enum QuickLookObject {
    case Text(String)
    case Int(Int64)
    case UInt(UInt64)
    case Float(Float32)
    case Double(Float64)
    case Image(Any)
    case Sound(Any)
    case Color(Any)
    case BezierPath(Any)
    case AttributedString(Any)
    case Rectangle(Float64, Float64, Float64, Float64)
    case Point(Float64, Float64)
    case Size(Float64, Float64)
    case Logical(Bool)
    case Range(UInt64, UInt64)
    case View(Any)
    case Sprite(Any)
    case URL(String)
}

extension QuickLookObject {

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
    init(reflecting subject: Any)
}


/// An *index* that can be offset by an arbitrary number of positions,
/// and can measure the distance to any reachable value, in O(1).
protocol RandomAccessIndexType : BidirectionalIndexType, Strideable {

    /// Return the minimum number of applications of `successor` or
    /// `predecessor` required to reach `other` from `self`.
    ///
    /// - Complexity: O(1).
    ///
    /// Axioms:
    ///
    ///     x.distanceTo(x.successor())) == 1
    ///     x.distanceTo(x.predecessor())) == -1
    ///     x.advancedBy(x.distanceTo(y)) == y
    func distanceTo(other: Self) -> Self.Distance

    /// Return `self` offset by `n` steps.
    ///
    /// - Returns: If `n > 0`, the result of applying `successor` to
    ///   `self` `n` times.  If `n < 0`, the result of applying
    ///   `predecessor` to `self` `n` times. Otherwise, `self`.
    ///
    /// - Complexity: O(1).
    ///
    /// Axioms:
    ///
    ///     x.advancedBy(0) == x
    ///     x.advancedBy(1) == x.successor()
    ///     x.advancedBy(-1) == x.predecessor()
    ///     x.distanceTo(x.advancedBy(m)) == m
    func advancedBy(n: Self.Distance) -> Self
}


/// The lazy `CollectionType` returned by `reverse(c)` where `c` is a
/// `CollectionType` with an `Index` conforming to `RandomAccessIndexType`.
struct RandomAccessReverseView<T : CollectionType where T.Index : RandomAccessIndexType> : CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType {

    /// A type that represents a valid position in the collection.
    ///
    /// Valid indices consist of the position of every element and a
    /// "past the end" position that's not valid for use as a subscript.
    typealias Index = ReverseRandomAccessIndex<T.Index>

    /// A type whose instances can produce the elements of this
    /// sequence, in order.
    typealias Generator = IndexingGenerator<RandomAccessReverseView<T>>

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> IndexingGenerator<RandomAccessReverseView<T>>

    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    var startIndex: ReverseRandomAccessIndex<T.Index> { get }

    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    var endIndex: ReverseRandomAccessIndex<T.Index> { get }
    subscript (position: ReverseRandomAccessIndex<T.Index>) -> T.Generator.Element { get }
}


/// A collection of consecutive discrete index values.
///
/// - parameter T: Is both the element type and the index type of the
///   collection.
///
/// Like other collections, a range containing one element has an
/// `endIndex` that is the successor of its `startIndex`; and an empty
/// range has `startIndex == endIndex`.
///
/// Axiom: for any `Range` `r`, `r[i] == i`.
///
/// Therefore, if `T` has a maximal value, it can serve as an
/// `endIndex`, but can never be contained in a `Range<T>`.
///
/// It also follows from the axiom above that `(-99..<100)[0] == 0`.
/// To prevent confusion (because some expect the result to be `-99`),
/// in a context where `T` is known to be an integer type,
/// subscripting with `T` is a compile-time error:
///
///     // error: could not find an overload for 'subscript'...
///     print(Range<Int>(start: -99, end: 100)[0])
///
/// However, subscripting that range still works in a generic context:
///
///     func brackets<T : ForwardIndexType>(x: Range<T>, i: T) -> T {
///       return x[i] // Just forward to subscript
///     }
///     print(brackets(Range<Int>(start:-99, end:100), 0)) // prints 0
struct Range<T : ForwardIndexType> : Equatable, CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType, CustomStringConvertible, CustomDebugStringConvertible {

    /// Construct a copy of `x`.
    init(_ x: Range<T>)

    /// Construct a range with `startIndex == start` and `endIndex ==
    /// end`.
    init(start: T, end: T)

    /// A type that represents a valid position in the collection.
    ///
    /// Valid indices consist of the position of every element and a
    /// "past the end" position that's not valid for use as a subscript.
    typealias Index = T
    subscript (position: T) -> T { get }
    subscript (_: T._DisabledRangeIndex) -> T { get }

    /// A type whose instances can produce the elements of this
    /// sequence, in order.
    typealias Generator = RangeGenerator<T>

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> RangeGenerator<T>

    /// The range's lower bound.
    ///
    /// Identical to `endIndex` in an empty range.
    var startIndex: T

    /// The range's upper bound.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    var endIndex: T

    /// A textual representation of `self`.
    var description: String { get }

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }
}

extension Range : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}


/// A generator over the elements of `Range<T>`.
struct RangeGenerator<T : ForwardIndexType> : GeneratorType, SequenceType {

    /// The type of element returned by `next()`.
    typealias Element = T

    /// Construct an instance that traverses the elements of `bounds`.
    init(_ bounds: Range<T>)

    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    mutating func next() -> T?

    /// A type whose instances can produce the elements of this
    /// sequence, in order.
    typealias Generator = RangeGenerator<T>

    /// `RangeGenerator` is also a `SequenceType`, so it
    /// `generate`'s a copy of itself.
    func generate() -> RangeGenerator<T>

    /// The lower bound of the remaining range.
    var startIndex: T

    /// The upper bound of the remaining range; not included in the
    /// generated sequence.
    var endIndex: T
}


/// A *collection* that supports replacement of an arbitrary subRange
/// of elements with the elements of another collection.
protocol RangeReplaceableCollectionType : ExtensibleCollectionType {

    /// Append `x` to `self`.
    ///
    /// Applying `successor()` to the index of the new element yields
    /// `self.endIndex`.
    ///
    /// - Complexity: Amortized O(1).
    mutating func append(x: Self.Generator.Element)

    /// Replace the given `subRange` of elements with `newElements`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`subRange.count`) if
    ///   `subRange.endIndex == self.endIndex` and `isEmpty(newElements)`,
    ///   O(`self.count` + `newElements.count`) otherwise.
    mutating func replaceRange<C : CollectionType where C.Generator.Element == Generator.Element>(subRange: Range<Self.Index>, with newElements: C)

    /// Insert `newElement` at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count`).
    ///
    /// Can be implemented as:
    ///
    ///     Swift.insert(&self, newElement, atIndex: i)
    mutating func insert(newElement: Self.Generator.Element, atIndex i: Self.Index)

    /// Insert `newElements` at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count + newElements.count`).
    ///
    /// Can be implemented as:
    ///
    ///     Swift.splice(&self, newElements, atIndex: i)
    mutating func splice<S : CollectionType where S.Generator.Element == Generator.Element>(newElements: S, atIndex i: Self.Index)

    /// Remove the element at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count`).
    ///
    /// Can be implemented as:
    ///
    ///     Swift.removeAtIndex(&self, i)
    mutating func removeAtIndex(i: Self.Index) -> Self.Generator.Element

    /// Remove the indicated `subRange` of elements.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count`).
    ///
    /// Can be implemented as:
    ///
    ///     Swift.removeRange(&self, subRange)
    mutating func removeRange(subRange: Range<Self.Index>)

    /// Remove all elements.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - parameter keepCapacity: If `true`, is a non-binding request to
    ///    avoid releasing storage, which can be a useful optimization
    ///    when `self` is going to be grown again.
    ///
    /// - Complexity: O(`self.count`).
    ///
    /// Can be implemented as:
    ///
    ///     Swift.removeAll(&self, keepCapacity: keepCapacity)
    mutating func removeAll(keepCapacity keepCapacity: Bool)
}


/// A byte-sized thing that isn't designed to interoperate with
/// any other types; it makes a decent parameter to `UnsafeMutablePointer` when
/// you just want to do bytewise pointer arithmetic.
struct RawByte {
}


/// Protocol for `NS_OPTIONS` imported from Objective-C.
protocol RawOptionSetType : _RawOptionSetType, BitwiseOperationsType, NilLiteralConvertible {
}


/// A type that can be converted to an associated "raw" type, then
/// converted back to produce an instance equivalent to the original.
protocol RawRepresentable {

    /// The "raw" type that can be used to represent all values of `Self`.
    ///
    /// Every distinct value of `self` has a corresponding unique
    /// value of `RawValue`, but `RawValue` may have representations
    /// that do not correspond to an value of `Self`.
    typealias RawValue

    /// Convert from a value of `RawValue`, yielding `nil` iff
    /// `rawValue` does not correspond to a value of `Self`.
    init?(rawValue: Self.RawValue)

    /// The corresponding value of the "raw" type.
    ///
    /// `Self(rawValue: self.rawValue)!` is equivalent to `self`.
    var rawValue: Self.RawValue { get }
}


/// Customizes the result of `reflect(x)`, where `x` is a conforming
/// type.
protocol Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}


/// A collection whose elements are all identical `T`s.
struct Repeat<T> : CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType {

    /// A type that represents a valid position in the collection.
    /// 
    /// Valid indices consist of the position of every element and a
    /// "past the end" position that's not valid for use as a subscript.
    typealias Index = Int

    /// Construct an instance that contains `count` elements having the
    /// value `repeatedValue`.
    init(count: Int, repeatedValue: T)

    /// Always zero, which is the index of the first element in a
    /// non-empty instance.
    var startIndex: Index { get }

    /// Always equal to `count`, which is one greater than the index of
    /// the last element in a non-empty instance.
    var endIndex: Index { get }

    /// Returns a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> IndexingGenerator<Repeat<T>>
    subscript (position: Int) -> T { get }

    /// The number of elements in this collection.
    var count: Int

    /// The value of every element in this collection.
    let repeatedValue: T
}


/// A wrapper for a `BidirectionalIndexType` that reverses its
/// direction of traversal.
struct ReverseBidirectionalIndex<I : BidirectionalIndexType> : ForwardIndexType, _Incrementable, Equatable, BidirectionalIndexType {

    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    func successor() -> ReverseBidirectionalIndex<I>

    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    func predecessor() -> ReverseBidirectionalIndex<I>
}


/// A wrapper for a `RandomAccessIndexType` that reverses its
/// direction of traversal.
struct ReverseRandomAccessIndex<I : RandomAccessIndexType> : ForwardIndexType, _Incrementable, Equatable, RandomAccessIndexType, BidirectionalIndexType, Strideable, Comparable, _Strideable {

    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    func successor() -> ReverseRandomAccessIndex<I>

    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    func predecessor() -> ReverseRandomAccessIndex<I>

    /// Return the minimum number of applications of `successor` or
    /// `predecessor` required to reach `other` from `self`.
    ///
    /// - Complexity: O(1).
    func distanceTo(other: ReverseRandomAccessIndex<I>) -> I.Distance

    /// Return `self` offset by `n` steps.
    ///
    /// - Returns: If `n > 0`, the result of applying `successor` to
    ///   `self` `n` times.  If `n < 0`, the result of applying
    ///   `predecessor` to `self` `-n` times. Otherwise, `self`.
    ///
    /// - Complexity: O(1).
    func advancedBy(amount: I.Distance) -> ReverseRandomAccessIndex<I>
}


/// A type that can be iterated with a `for`...`in` loop.
///
/// `SequenceType` makes no requirement on conforming types regarding
/// whether they will be destructively "consumed" by iteration.  To
/// ensure non-destructive iteration, constrain your *sequence* to
/// `CollectionType`.
protocol SequenceType {

    /// A type that provides the *sequence*'s iteration interface and
    /// encapsulates its iteration state.
    typealias Generator : GeneratorType

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> Self.Generator

    /// Return a value less than or equal to the number of elements in
    /// `self`, **nondestructively**.
    ///
    /// - Complexity: O(N).
    func underestimateCount() -> Int

    /// Return an `Array` containing the results of mapping `transform`
    /// over `self`.
    ///
    /// - Complexity: O(N).
    func map<T>(@noescape transform: (Self.Generator.Element) -> T) -> [T]

    /// Return an `Array` containing the elements of `self`,
    /// in order, that satisfy the predicate `includeElement`.
    func filter(@noescape includeElement: (Self.Generator.Element) -> Bool) -> [Self.Generator.Element]
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
    func sort() -> [Self.Generator.Element]
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
    func sort(isOrderedBefore: (Self.Generator.Element, Self.Generator.Element) -> Bool) -> [Self.Generator.Element]
}

extension SequenceType {

    /// Return a value less than or equal to the number of elements in
    /// `self`, **nondestructively**.
    ///
    /// - Complexity: O(N).
    func underestimateCount() -> Int
}

extension SequenceType {

    /// Return an `Array` containing the results of mapping `transform`
    /// over `self`.
    ///
    /// - Complexity: O(N).
    func map<T>(@noescape transform: (Self.Generator.Element) -> T) -> [T]
}

extension SequenceType {

    /// Return an `Array` containing the elements of `self`,
    /// in order, that satisfy the predicate `includeElement`.
    func filter(@noescape includeElement: (Self.Generator.Element) -> Bool) -> [Self.Generator.Element]
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
    func enumerate() -> EnumerateSequence<Self>
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
    func minElement(@noescape isOrderedBefore: (Self.Generator.Element, Self.Generator.Element) -> Bool) -> Self.Generator.Element?

    /// Returns the maximum element in `self` or `nil` if the sequence is empty.
    ///
    /// - Complexity: O(`elements.count`).
    ///   
    /// - Requires: `isOrderedBefore` is a
    ///   [strict weak ordering](http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings).
    ///   over `self`.
    func maxElement(@noescape isOrderedBefore: (Self.Generator.Element, Self.Generator.Element) -> Bool) -> Self.Generator.Element?
}

extension SequenceType where Generator.Element : Comparable {

    /// Returns the minimum element in `self` or `nil` if the sequence is empty.
    ///
    /// - Complexity: O(`elements.count`).
    ///
    /// 
    func minElement() -> Self.Generator.Element?

    /// Returns the maximum element in `self` or `nil` if the sequence is empty.
    ///
    /// - Complexity: O(`elements.count`).
    ///   
    func maxElement() -> Self.Generator.Element?
}

extension SequenceType {

    /// Return true iff `self` begins with elements equivalent to those of
    /// `other`, using `isEquivalent` as the equivalence test.  Return true if
    /// `other` is empty.
    ///
    /// - Requires: `isEquivalent` is an
    ///   [equivalence relation](http://en.wikipedia.org/wiki/Equivalence_relation).
    func startsWith<OtherSequence : SequenceType where OtherSequence.Generator.Element == Generator.Element>(other: OtherSequence, @noescape isEquivalent: (Self.Generator.Element, Self.Generator.Element) -> Bool) -> Bool
}

extension SequenceType where Generator.Element : Equatable {

    /// Return true iff the the initial elements of `self` are equal to `prefix`.
    /// Return true if `other` is empty.
    func startsWith<OtherSequence : SequenceType where OtherSequence.Generator.Element == Generator.Element>(other: OtherSequence) -> Bool
}

extension SequenceType {

    /// Return true iff `self` and `other` contain equivalent elements, using
    /// `isEquivalent` as the equivalence test.
    ///
    /// - Requires: `isEquivalent` is an
    ///   [equivalence relation](http://en.wikipedia.org/wiki/Equivalence_relation).
    func elementsEqual<OtherSequence : SequenceType where OtherSequence.Generator.Element == Generator.Element>(other: OtherSequence, @noescape isEquivalent: (Self.Generator.Element, Self.Generator.Element) -> Bool) -> Bool
}

extension SequenceType where Generator.Element : Equatable {

    /// Return `true` iff `self` and `other` contain the same elements in the
    /// same order.
    func elementsEqual<OtherSequence : SequenceType where OtherSequence.Generator.Element == Generator.Element>(other: OtherSequence) -> Bool
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
    func lexicographicalCompare<OtherSequence : SequenceType where OtherSequence.Generator.Element == Generator.Element>(other: OtherSequence, @noescape isOrderedBefore: (Self.Generator.Element, Self.Generator.Element) -> Bool) -> Bool
}

extension SequenceType where Generator.Element : Comparable {

    /// Return true iff `self` precedes `other` in a lexicographical ("dictionary")
    /// ordering, using "<" as the comparison between elements.
    ///
    /// - Note: This method implements the mathematical notion of lexicographical
    ///   ordering, which has no connection to Unicode.  If you are sorting strings
    ///   to present to the end-user, you should use `String` APIs that perform
    /// localized comparison.
    func lexicographicalCompare<OtherSequence : SequenceType where OtherSequence.Generator.Element == Generator.Element>(other: OtherSequence) -> Bool
}

extension SequenceType where Generator.Element : Equatable {

    /// Return `true` iff `x` is in `self`.
    func contains(element: Self.Generator.Element) -> Bool
}

extension SequenceType {

    /// Return `true` iff an element in `self` satisfies `predicate`.
    func contains(@noescape predicate: (Self.Generator.Element) -> Bool) -> Bool
}

extension SequenceType {

    /// Return the result of repeatedly calling `combine` with an
    /// accumulated value initialized to `initial` and each element of
    /// `self`, in turn, i.e. return
    /// `combine(combine(...combine(combine(initial, self[0]),
    /// self[1]),...self[count-2]), self[count-1])`.
    func reduce<T>(initial: T, @noescape combine: (T, Self.Generator.Element) -> T) -> T
}

extension SequenceType {

    /// Return an `Array` containing the elements of `self` in reverse
    /// order.
    func reverse() -> [Self.Generator.Element]
}

extension SequenceType {

    /// Return an `Array` containing concatenated results of mapping `transform`
    /// over `self`.
    func flatMap<S : SequenceType>(@noescape transform: (Self.Generator.Element) -> S) -> [S.Generator.Element]
}

extension SequenceType {

    /// Return an `Array` containing the non-nil results of mapping `transform`
    /// over `self`.
    func flatMap<T>(@noescape transform: (Self.Generator.Element) -> T?) -> [T]
}


/// A collection of unique `T` instances with no defined ordering.
struct Set<T : Hashable> : Hashable, Equatable, CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType, ArrayLiteralConvertible {
    typealias Element = T
    typealias Index = SetIndex<T>
    typealias GeneratorType = SetGenerator<T>

    /// Create an empty set with at least the given number of
    /// elements worth of storage.  The actual capacity will be the
    /// smallest power of 2 that's >= `minimumCapacity`.
    init(minimumCapacity: Int)

    /// The position of the first element in a non-empty set.
    ///
    /// This is identical to `endIndex` in an empty set.
    ///
    /// - Complexity: Amortized O(1) if `self` does not wrap a bridged
    ///   `NSSet`, O(N) otherwise.
    var startIndex: SetIndex<T> { get }

    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    ///
    /// - Complexity: Amortized O(1) if `self` does not wrap a bridged
    ///   `NSSet`, O(N) otherwise.
    var endIndex: SetIndex<T> { get }

    /// Returns `true` if the set contains a member.
    func contains(member: T) -> Bool

    /// Returns the `Index` of a given member, or `nil` if the member is not
    /// present in the set.
    func indexOf(member: T) -> SetIndex<T>?

    /// Insert a member into the set.
    mutating func insert(member: T)

    /// Remove the member from the set and return it if it was present.
    mutating func remove(member: T) -> T?

    /// Remove the member referenced by the given index.
    mutating func removeAtIndex(index: SetIndex<T>)

    /// Erase all the elements.  If `keepCapacity` is `true`, `capacity`
    /// will not decrease.
    mutating func removeAll(keepCapacity keepCapacity: Bool = default)

    /// Remove a member from the set and return it.
    ///
    /// - Requires: `count > 0`.
    mutating func removeFirst() -> T

    /// The number of members in the set.
    ///
    /// - Complexity: O(1).
    var count: Int { get }
    subscript (position: SetIndex<T>) -> T { get }

    /// Return a *generator* over the members.
    ///
    /// - Complexity: O(1).
    func generate() -> SetGenerator<T>
    init(arrayLiteral elements: T...)

    /// Create an empty `Set`.
    init()

    /// Create a `Set` from a finite sequence of items.
    init<S : SequenceType where S.Generator.Element == T>(_ sequence: S)

    /// Returns true if the set is a subset of a finite sequence as a `Set`.
    func isSubsetOf<S : SequenceType where S.Generator.Element == T>(sequence: S) -> Bool

    /// Returns true if the set is a subset of a finite sequence as a `Set`
    /// but not equal.
    func isStrictSubsetOf<S : SequenceType where S.Generator.Element == T>(sequence: S) -> Bool

    /// Returns true if the set is a superset of a finite sequence as a `Set`.
    func isSupersetOf<S : SequenceType where S.Generator.Element == T>(sequence: S) -> Bool

    /// Returns true if the set is a superset of a finite sequence as a `Set`
    /// but not equal.
    func isStrictSupersetOf<S : SequenceType where S.Generator.Element == T>(sequence: S) -> Bool

    /// Returns true if no members in the set are in a finite sequence as a `Set`.
    func isDisjointWith<S : SequenceType where S.Generator.Element == T>(sequence: S) -> Bool

    /// Return a new `Set` with items in both this set and a finite sequence.
    func union<S : SequenceType where S.Generator.Element == T>(sequence: S) -> Set<T>

    /// Insert elements of a finite sequence into this `Set`.
    mutating func unionInPlace<S : SequenceType where S.Generator.Element == T>(sequence: S)

    /// Return a new set with elements in this set that do not occur
    /// in a finite sequence.
    func subtract<S : SequenceType where S.Generator.Element == T>(sequence: S) -> Set<T>

    /// Remove all members in the set that occur in a finite sequence.
    mutating func subtractInPlace<S : SequenceType where S.Generator.Element == T>(sequence: S)

    /// Return a new set with elements common to this set and a finite sequence.
    func intersect<S : SequenceType where S.Generator.Element == T>(sequence: S) -> Set<T>

    /// Remove any members of this set that aren't also in a finite sequence.
    mutating func intersectInPlace<S : SequenceType where S.Generator.Element == T>(sequence: S)

    /// Return a new set with elements that are either in the set or a finite
    /// sequence but do not occur in both.
    func exclusiveOr<S : SequenceType where S.Generator.Element == T>(sequence: S) -> Set<T>

    /// For each element of a finite sequence, remove it from the set if it is a
    /// common element, otherwise add it to the set. Repeated elements of the
    /// sequence will be ignored.
    mutating func exclusiveOrInPlace<S : SequenceType where S.Generator.Element == T>(sequence: S)
    var hashValue: Int { get }

    /// `true` if the set is empty.
    var isEmpty: Bool { get }

    /// The first element obtained when iterating, or `nil` if `self` is
    /// empty.  Equivalent to `self.generate().next()`.
    var first: T? { get }
}

extension Set : CustomStringConvertible, CustomDebugStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }
}

extension Set : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
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
protocol SetAlgebraType : Equatable, ArrayLiteralConvertible {
    typealias DefaultImplementations = ()

    /// A type for which `Self` provides a containment test.
    typealias Element

    /// Creates an empty set.
    ///
    /// - Equivalent to `[] as Self`
    init()

    /// Returns `true` if `self` contains `member`.
    ///
    /// - Equivalent to `self.intersect([member]) == [member]`
    func contains(member: Self.Element) -> Bool

    /// Returns the set of elements contained in `self`, in `other`, or in
    /// both `self` and `other`.
    func union(other: Self) -> Self

    /// Returns the set of elements contained in both `self` and `other`.
    func intersect(other: Self) -> Self

    /// Returns the set of elements contained in `self` or in `other`,
    /// but not in both `self` and `other`.
    func exclusiveOr(other: Self) -> Self

    /// If `member` is not already contained in `self`, inserts it.
    ///
    /// - Equivalent to `self.unionInPlace([member])`
    /// - Postcondition: `self.contains(member)`
    mutating func insert(member: Self.Element)

    /// If `member` is contained in `self`, removes and returns it.
    /// Otherwise, removes all elements subsumed by `member` and returns
    /// `nil`.
    ///
    /// - Postcondition: `self.intersect([member]).isEmpty`
    mutating func remove(member: Self.Element) -> Self.Element?

    /// Insert all elements of `other` into `self`.
    ///
    /// - Equivalent to replacing `self` with `self.union(other)`.
    /// - Postcondition: `self.isSupersetOf(other)`
    mutating func unionInPlace(other: Self)

    /// Removes all elements of `self` that are not also present in
    /// `other`.
    ///
    /// - Equivalent to replacing `self` with `self.intersect(other)`
    /// - Postcondition: `self.isSubsetOf(other)`
    mutating func intersectInPlace(other: Self)

    /// Replaces `self` with a set containing all elements contained in
    /// either `self` or `other`, but not both.
    ///
    /// - Equivalent to replacing `self` with `self.exclusiveOr(other)`
    mutating func exclusiveOrInPlace(other: Self)

    /// Return true iff `self.intersect(other).isEmpty`.
    func subtract(other: Self) -> Self

    /// Return true iff every element of `self` is contained in `other`.
    func isSubsetOf(other: Self) -> Bool

    /// Return true iff `self.intersect(other).isEmpty`.
    func isDisjointWith(other: Self) -> Bool

    /// Return true iff every element of `other` is contained in `self`.
    func isSupersetOf(other: Self) -> Bool

    /// Returns true iff `self.contains(e)` is `false` for all `e`.
    var isEmpty: Bool { get }

    /// Creates the set containing all elements of `sequence`.
    init<S : SequenceType where S.Generator.Element == Element>(_ sequence: S)

    /// Removes all elements of `other` from `self`.
    ///
    /// - Equivalent to replacing `self` with `self.subtract(other)`.
    mutating func subtractInPlace(other: Self)

    /// Returns `true` iff `a` subsumes `b`.
    ///
    /// - Equivalent to `([a] as Self).isSupersetOf([b])`
    static func element(a: Self.Element, subsumes b: Self.Element) -> Bool

    /// Returns `true` iff `a` is disjoint with `b`.
    ///
    /// Two elements are disjoint when neither one subsumes the other.
    ///
    /// - SeeAlso: `Self.element(_, subsumes:_)`
    static func element(a: Self.Element, isDisjointWith b: Self.Element) -> Bool
}

extension SetAlgebraType where DefaultImplementations == () {

    /// Creates the set containing all elements of `sequence`.
    convenience init<S : SequenceType where S.Generator.Element == Element>(_ sequence: S)

    /// Creates a set containing all elements of the given `arrayLiteral`.
    ///
    /// This initializer allows an array literal containing
    /// `Self.Element` to represent an instance of the set, wherever it
    /// is implied by the type context.
    convenience init(arrayLiteral: Self.Element...)

    /// Removes all elements of `other` from `self`.
    ///
    /// - Equivalent to replacing `self` with `self.subtract(other)`.
    mutating func subtractInPlace(other: Self)

    /// Returns true iff every element of `self` is contained in `other`.
    func isSubsetOf(other: Self) -> Bool

    /// Returns true iff every element of `other` is contained in `self`.
    func isSupersetOf(other: Self) -> Bool

    /// Returns true iff `self.intersect(other).isEmpty`.
    func isDisjointWith(other: Self) -> Bool

    /// Returns true iff `self.intersect(other).isEmpty`.
    func subtract(other: Self) -> Self

    /// Returns true iff `self.contains(e)` is `false` for all `e`.
    var isEmpty: Bool { get }

    /// Returns true iff every element of `other` is contained in `self`
    /// and `self` contains an element that is not contained in `other`.
    func isStrictSupersetOf(other: Self) -> Bool

    /// Return true iff every element of `self` is contained in `other`
    /// and `other` contains an element that is not contained in `self`.
    func isStrictSubsetOf(other: Self) -> Bool

    /// Returns `true` iff `a` subsumes `b`.
    ///
    /// - Equivalent to `([a] as Self).isSupersetOf([b])`
    static func element(a: Self.Element, subsumes b: Self.Element) -> Bool

    /// Returns `true` iff `a` is disjoint with `b`.
    ///
    /// Two elements are disjoint when neither one subsumes the other.
    ///
    /// - SeeAlso: `Self.element(_, subsumes:_)`
    static func element(a: Self.Element, isDisjointWith b: Self.Element) -> Bool
}


/// A generator over the members of a `Set<T>`.
struct SetGenerator<T : Hashable> : GeneratorType {

    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: No preceding call to `self.next()` has returned `nil`.
    mutating func next() -> T?
}


/// Used to access the members in an instance of `Set<T>`.
struct SetIndex<T : Hashable> : ForwardIndexType, _Incrementable, Equatable, Comparable {

    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    func successor() -> SetIndex<T>
}


/// A set of common requirements for Swift's signed integer types.
protocol SignedIntegerType : _SignedIntegerType, IntegerType {
}


/// Instances of conforming types can be subtracted, arithmetically
/// negated, and initialized from `0`.
///
/// Axioms:
///
/// - `x - 0 == x`
/// - `-x == 0 - x`
/// - `-(-x) == x`
protocol SignedNumberType : Comparable, IntegerLiteralConvertible {

    /// Return the result of negating `x`.
    prefix func -(x: Self) -> Self

    /// Return the difference between `lhs` and `rhs`.
    func -(lhs: Self, rhs: Self) -> Self
}


/// A type-erased sink.
///
/// Forwards operations to an arbitrary underlying sink with the same
/// `Element` type, hiding the specifics of the underlying sink type.
struct SinkOf<T> : SinkType {

    /// Construct an instance whose `put(x)` calls `putElement(x)`.
    init(_ putElement: (T) -> ())

    /// Construct an instance whose `put(x)` calls `base.put(x)`.
    init<S : SinkType where S.Element == T>(_ base: S)

    /// Write `x` to this sink.
    func put(x: T)
}


/// Instances of conforming types are effectively functions with the
/// signature `(Element) -> Void`.
///
/// Useful mainly when the optimizer's ability to specialize generics
/// outstrips its ability to specialize ordinary closures.  For
/// example, you may find that instead of:
///
///     func f(g: (X)->Void) { ... g(a) ...}
///
/// the following generates better code:
///
///     func f<T : SinkType where T.Element == X>(g: T) { ... g.put(a) ...}
protocol SinkType {

    /// The type of element to be written to this sink.
    typealias Element

    /// Write `x` to this sink.
    mutating func put(x: Self.Element)
}


/// A *collection* from which a sub-range of elements (a "slice")
/// can be efficiently extracted.
protocol Sliceable : _Sliceable {

    /// The *collection* type that represents a sub-range of elements.
    ///
    /// Though it can't currently be enforced by the type system, the
    /// `SubSlice` type in a concrete implementation of `Sliceable`
    /// should also be `Sliceable`.
    typealias SubSlice : _Sliceable
    subscript (bounds: Range<Self.Index>) -> Self.SubSlice { get }
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
struct StaticString : UnicodeScalarLiteralConvertible, ExtendedGraphemeClusterLiteralConvertible, StringLiteralConvertible, CustomStringConvertible, CustomDebugStringConvertible, Reflectable {

    /// A pointer to the beginning of UTF-8 code units.
    ///
    /// - Requires: `self` stores a pointer to either ASCII or UTF-8 code
    ///   units.
    var utf8Start: UnsafePointer<UInt8> { get }

    /// The stored Unicode scalar value.
    ///
    /// - Requires: `self` stores a single Unicode scalar value.
    var unicodeScalar: UnicodeScalar { get }

    /// If `self` stores a pointer to ASCII or UTF-8 code units, the
    /// length in bytes of that data.
    ///
    /// If `self` stores a single Unicode scalar value, the value of
    /// `byteSize` is unspecified.
    var byteSize: Word { get }

    /// `true` iff `self` stores a pointer to ASCII or UTF-8 code units.
    var hasPointerRepresentation: Bool { get }

    /// `true` if `self` stores a pointer to ASCII code units.
    ///
    /// If `self` stores a single Unicode scalar value, the value of
    /// `isASCII` is unspecified.
    var isASCII: Bool { get }

    /// Invoke `body` with a buffer containing the UTF-8 code units of
    /// `self`.
    ///
    /// This method works regardless of what `self` stores.
    func withUTF8Buffer<R>(@noescape body: (UnsafeBufferPointer<UInt8>) -> R) -> R

    /// Return a `String` representing the same sequence of Unicode
    /// scalar values as `self` does.
    var stringValue: String { get }

    /// Create an empty instance.
    init()
    init(_builtinUnicodeScalarLiteral value: Builtin.Int32)

    /// Create an instance initialized to `value`.
    init(unicodeScalarLiteral value: StaticString)
    init(_builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer, byteSize: Builtin.Word, isASCII: Builtin.Int1)

    /// Create an instance initialized to `value`.
    init(extendedGraphemeClusterLiteral value: StaticString)
    init(_builtinStringLiteral start: Builtin.RawPointer, byteSize: Builtin.Word, isASCII: Builtin.Int1)

    /// Create an instance initialized to `value`.
    init(stringLiteral value: StaticString)

    /// A textual representation of `self`.
    var description: String { get }

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }
    func getMirror() -> MirrorType
}


/// A source of text streaming operations.  `Streamable` instances can
/// be written to any *output stream*.
///
/// For example: `String`, `Character`, `UnicodeScalar`.
protocol Streamable {

    /// Write a textual representation of `self` into `target`.
    func writeTo<Target : OutputStreamType>(inout target: Target)
}


/// A `SequenceType` of values formed by striding over a closed interval.
struct StrideThrough<T : Strideable> : SequenceType {

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> StrideThroughGenerator<T>
}

extension StrideThrough : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}


/// A GeneratorType for `StrideThrough<T>`.
struct StrideThroughGenerator<T : Strideable> : GeneratorType {

    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    mutating func next() -> T?
}


/// A `SequenceType` of values formed by striding over a half-open interval.
struct StrideTo<T : Strideable> : SequenceType {

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> StrideToGenerator<T>
}

extension StrideTo : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}


/// A GeneratorType for `StrideTo<T>`.
struct StrideToGenerator<T : Strideable> : GeneratorType {

    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    mutating func next() -> T?
}


/// Conforming types are notionally continuous, one-dimensional
/// values that can be offset and measured.
///
/// - SeeAlso: `stride(from: to: by:)` and `stride(from: through: by:)`
protocol Strideable : Comparable, _Strideable {
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
///     b.extend("bar")
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
struct String {

    /// An empty `String`.
    init()
}

extension String {
    typealias Index = String.CharacterView.Index

    /// The position of the first `Character` in `self.characters` if
    /// `self` is non-empty; identical to `endIndex` otherwise.
    var startIndex: Index { get }

    /// The "past the end" position in `self.characters`.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    var endIndex: Index { get }
    subscript (i: Index) -> Character { get }

    /// Return a *generator* over the `Character`s.
    ///
    /// - Complexity: O(1).
    func generate() -> IndexingGenerator<String.CharacterView>
}

extension String {

    /// A `String`'s collection of `Character`s ([extended grapheme
    /// clusters](http://www.unicode.org/glossary/#extended_grapheme_cluster))
    /// elements.
    struct CharacterView {

        /// Create a view of the `Character`s in `text`.
        init(_ text: String)
    }

    /// A collection of `Characters` representing the `String`'s
    /// [extended grapheme
    /// clusters](http://www.unicode.org/glossary/#extended_grapheme_cluster).
    var characters: String.CharacterView { get }

    /// Efficiently mutate `self` by applying `body` to its `characters`.
    ///
    /// - Warning: Do not rely on anything about `self` (the `String`
    ///   that is the target of this method) during the execution of
    ///   `body`: it may not appear to have its correct value.  Instead,
    ///   use only the `String.CharacterView` argument to `body`.
    mutating func withMutableCharacters<R>(body: (inout String.CharacterView) -> R) -> R

    /// Construct the `String` corresponding to the given sequence of
    /// Unicode scalars.
    init(_ characters: String.CharacterView)
}

extension String {

    /// A collection of [Unicode scalar values](http://www.unicode.org/glossary/#unicode_scalar_value) that
    /// encode a `String` .
    struct UnicodeScalarView : Sliceable, _Sliceable, CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType, Reflectable, CustomStringConvertible, CustomDebugStringConvertible {

        /// A position in a `String.UnicodeScalarView`.
        struct Index : ForwardIndexType, _Incrementable, Equatable, BidirectionalIndexType, Comparable {

            /// Returns the next consecutive value after `self`.
            ///
            /// - Requires: The next value is representable.
            func successor() -> String.UnicodeScalarView.Index

            /// Returns the previous consecutive value before `self`.
            ///
            /// - Requires: The previous value is representable.
            func predecessor() -> String.UnicodeScalarView.Index
        }

        /// The position of the first `UnicodeScalar` if the `String` is
        /// non-empty; identical to `endIndex` otherwise.
        var startIndex: String.UnicodeScalarView.Index { get }

        /// The "past the end" position.
        ///
        /// `endIndex` is not a valid argument to `subscript`, and is always
        /// reachable from `startIndex` by zero or more applications of
        /// `successor()`.
        var endIndex: String.UnicodeScalarView.Index { get }
        subscript (position: String.UnicodeScalarView.Index) -> UnicodeScalar { get }
        subscript (r: Range<String.UnicodeScalarView.Index>) -> String.UnicodeScalarView { get }

        /// A type whose instances can produce the elements of this
        /// sequence, in order.
        struct Generator : GeneratorType {

            /// Advance to the next element and return it, or `nil` if no next
            /// element exists.
            ///
            /// - Requires: No preceding call to `self.next()` has returned
            ///   `nil`.
            mutating func next() -> UnicodeScalar?
        }

        /// Return a *generator* over the `UnicodeScalar`s that comprise
        /// this *sequence*.
        ///
        /// - Complexity: O(1).
        func generate() -> String.UnicodeScalarView.Generator

        /// Returns a mirror that reflects `self`.
        func getMirror() -> MirrorType
        var description: String { get }
        var debugDescription: String { get }
    }

    /// Construct the `String` corresponding to the given sequence of
    /// Unicode scalars.
    init(_ unicodeScalars: String.UnicodeScalarView)

    /// The index type for subscripting a `String`'s `.unicodeScalars`
    /// view.
    typealias UnicodeScalarIndex = String.UnicodeScalarView.Index
}

extension String {

    /// A collection of UTF-16 code units that encodes a `String` value.
    struct UTF16View : Sliceable, _Sliceable, CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType, Reflectable, CustomStringConvertible, CustomDebugStringConvertible {
        struct Index {
        }

        /// The position of the first code unit if the `String` is
        /// non-empty; identical to `endIndex` otherwise.
        var startIndex: String.UTF16View.Index { get }

        /// The "past the end" position.
        ///
        /// `endIndex` is not a valid argument to `subscript`, and is always
        /// reachable from `startIndex` by zero or more applications of
        /// `successor()`.
        var endIndex: String.UTF16View.Index { get }
        func generate() -> IndexingGenerator<String.UTF16View>
        subscript (i: String.UTF16View.Index) -> CodeUnit { get }
        subscript (subRange: Range<String.UTF16View.Index>) -> String.UTF16View { get }

        /// Returns a mirror that reflects `self`.
        func getMirror() -> MirrorType
        var description: String { get }
        var debugDescription: String { get }
    }

    /// A UTF-16 encoding of `self`.
    var utf16: String.UTF16View { get }

    /// Construct the `String` corresponding to the given sequence of
    /// UTF-16 code units.  If `utf16` contains unpaired surrogates, the
    /// result is `nil`.
    init?(_ utf16: String.UTF16View)

    /// The index type for subscripting a `String`'s `utf16` view.
    typealias UTF16Index = String.UTF16View.Index
}

extension String {

    /// A collection of UTF-8 code units that encodes a `String` value.
    struct UTF8View : CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType, Reflectable, CustomStringConvertible, CustomDebugStringConvertible {

        /// A position in a `String.UTF8View`.
        struct Index : ForwardIndexType, _Incrementable, Equatable {

            /// Returns the next consecutive value after `self`.
            ///
            /// - Requires: The next value is representable.
            func successor() -> String.UTF8View.Index
        }

        /// The position of the first code unit if the `String` is
        /// non-empty; identical to `endIndex` otherwise.
        var startIndex: String.UTF8View.Index { get }

        /// The "past the end" position.
        ///
        /// `endIndex` is not a valid argument to `subscript`, and is always
        /// reachable from `startIndex` by zero or more applications of
        /// `successor()`.
        var endIndex: String.UTF8View.Index { get }
        subscript (position: String.UTF8View.Index) -> CodeUnit { get }
        subscript (subRange: Range<String.UTF8View.Index>) -> String.UTF8View { get }

        /// Returns a *generator* over the code points that comprise this
        /// *sequence*.
        ///
        /// - Complexity: O(1).
        func generate() -> IndexingGenerator<String.UTF8View>

        /// Returns a mirror that reflects `self`.
        func getMirror() -> MirrorType
        var description: String { get }
        var debugDescription: String { get }
    }

    /// A UTF-8 encoding of `self`.
    var utf8: String.UTF8View { get }

    /// A contiguously-stored nul-terminated UTF-8 representation of
    /// `self`.
    ///
    /// To access the underlying memory, invoke
    /// `withUnsafeBufferPointer` on the `ContiguousArray`.
    var nulTerminatedUTF8: ContiguousArray<CodeUnit> { get }

    /// Construct the `String` corresponding to the given sequence of
    /// UTF-8 code units.  If `utf8` contains unpaired surrogates, the
    /// result is `nil`.
    init?(_ utf8: String.UTF8View)

    /// The index type for subscripting a `String`'s `.utf8` view.
    typealias UTF8Index = String.UTF8View.Index
}

extension String {

    /// Creates a new `String` by copying the nul-terminated UTF-8 data
    /// referenced by a `CString`.
    ///
    /// Returns `nil` if the `CString` is `NULL` or if it contains ill-formed
    /// UTF-8 code unit sequences.
    static func fromCString(cs: UnsafePointer<CChar>) -> String?

    /// Creates a new `String` by copying the nul-terminated UTF-8 data
    /// referenced by a `CString`.
    ///
    /// Returns `nil` if the `CString` is `NULL`.  If `CString` contains
    /// ill-formed UTF-8 code unit sequences, replaces them with replacement
    /// characters (U+FFFD).
    static func fromCStringRepairingIllFormedUTF8(cs: UnsafePointer<CChar>) -> (String?, hadError: Bool)
}

extension String {

    /// Construct an instance containing just the given `Character`.
    init(_ c: Character)
}

extension String {

    /// Invoke `f` on the contents of this string, represented as
    /// a nul-terminated array of char, ensuring that the array's
    /// lifetime extends through the execution of `f`.
    func withCString<Result>(@noescape f: UnsafePointer<Int8> -> Result) -> Result
}

extension String : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension String : OutputStreamType {

    /// Append `other` to this stream.
    mutating func write(other: String)
}

extension String : Streamable {

    /// Write a textual representation of `self` into `target`.
    func writeTo<Target : OutputStreamType>(inout target: Target)
}



extension String {
    init(_builtinUnicodeScalarLiteral value: Builtin.Int32)
}

extension String : UnicodeScalarLiteralConvertible {

    /// Create an instance initialized to `value`.
    init(unicodeScalarLiteral value: String)
}

extension String {
    init(_builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer, byteSize: Builtin.Word, isASCII: Builtin.Int1)
}

extension String : ExtendedGraphemeClusterLiteralConvertible {

    /// Create an instance initialized to `value`.
    init(extendedGraphemeClusterLiteral value: String)
}

extension String {
    init(_builtinUTF16StringLiteral start: Builtin.RawPointer, numberOfCodeUnits: Builtin.Word)
}

extension String {
    init(_builtinStringLiteral start: Builtin.RawPointer, byteSize: Builtin.Word, isASCII: Builtin.Int1)
}

extension String : StringLiteralConvertible {

    /// Create an instance initialized to `value`.
    init(stringLiteral value: String)
}

extension String : CustomDebugStringConvertible {

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }
}



extension String : Equatable {
}

extension String : Comparable {
}



extension String {

    /// Append the elements of `other` to `self`.
    mutating func extend(other: String)

    /// Append `x` to `self`.
    ///
    /// - Complexity: Amortized O(1).
    mutating func append(x: UnicodeScalar)
}

extension String : Hashable {

    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    var hashValue: Int { get }
}



extension String {
    subscript (subRange: Range<Index>) -> String { get }
}

extension String {
    mutating func reserveCapacity(n: Int)
    mutating func append(c: Character)
    mutating func extend<S : SequenceType where S.Generator.Element == Character>(newElements: S)

    /// Create an instance containing `characters`.
    init<S : SequenceType where S.Generator.Element == Character>(_ characters: S)
}

extension String {

    /// Interpose `self` between every pair of consecutive `elements`,
    /// then concatenate the result.  For example:
    ///
    ///     "-|-".join(["foo", "bar", "baz"]) // "foo-|-bar-|-baz"
    func join<S : SequenceType where S.Generator.Element == String>(elements: S) -> String
}

extension String {

    /// Replace the given `subRange` of elements with `newElements`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`subRange.count`) if `subRange.endIndex
    ///   == self.endIndex` and `isEmpty(newElements)`, O(N) otherwise.
    mutating func replaceRange<C : CollectionType where C.Generator.Element == Character>(subRange: Range<Index>, with newElements: C)

    /// Replace the given `subRange` of elements with `newElements`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`subRange.count`) if `subRange.endIndex
    ///   == self.endIndex` and `isEmpty(newElements)`, O(N) otherwise.
    mutating func replaceRange(subRange: Range<Index>, with newElements: String)

    /// Insert `newElement` at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count`).
    mutating func insert(newElement: Character, atIndex i: Index)

    /// Insert `newElements` at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count + newElements.count`).
    mutating func splice<S : CollectionType where S.Generator.Element == Character>(newElements: S, atIndex i: Index)

    /// Remove and return the element at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count`).
    mutating func removeAtIndex(i: Index) -> Character

    /// Remove the indicated `subRange` of characters.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count`).
    mutating func removeRange(subRange: Range<Index>)

    /// Remove all characters.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - parameter keepCapacity: If `true`, prevents the release of
    ///   allocated storage, which can be a useful optimization
    ///   when `self` is going to be grown again.
    mutating func removeAll(keepCapacity keepCapacity: Bool = default)
}

extension String {
    var lowercaseString: String { get }
    var uppercaseString: String { get }
}





extension String : StringInterpolationConvertible {

    /// Create an instance by concatenating the elements of `strings`.
    init(stringInterpolation strings: String...)

    /// Create an instance containing `expr`'s `print` representation.
    init<T>(stringInterpolationSegment expr: T)
    init(stringInterpolationSegment expr: String)
    init(stringInterpolationSegment expr: Character)
    init(stringInterpolationSegment expr: UnicodeScalar)
    init(stringInterpolationSegment expr: Bool)
    init(stringInterpolationSegment expr: Float32)
    init(stringInterpolationSegment expr: Float64)
    init(stringInterpolationSegment expr: UInt8)
    init(stringInterpolationSegment expr: Int8)
    init(stringInterpolationSegment expr: UInt16)
    init(stringInterpolationSegment expr: Int16)
    init(stringInterpolationSegment expr: UInt32)
    init(stringInterpolationSegment expr: Int32)
    init(stringInterpolationSegment expr: UInt64)
    init(stringInterpolationSegment expr: Int64)
    init(stringInterpolationSegment expr: UInt)
    init(stringInterpolationSegment expr: Int)
}

extension String {

    /// Construct an instance that is the concatenation of `count` copies
    /// of `repeatedValue`.
    init(count: Int, repeatedValue c: Character)

    /// Construct an instance that is the concatenation of `count` copies
    /// of `Character(repeatedValue)`.
    init(count: Int, repeatedValue c: UnicodeScalar)

    /// `true` iff `self` contains no characters.
    var isEmpty: Bool { get }
}



extension String {

    /// Returns `true` iff `self` begins with `prefix`.
    func hasPrefix(prefix: String) -> Bool

    /// Returns `true` iff `self` ends with `suffix`.
    func hasSuffix(suffix: String) -> Bool
}

extension String {

    /// Create an instance representing `v` in base 10.
    init<T : _SignedIntegerType>(_ v: T)

    /// Create an instance representing `v` in base 10.
    init<T : _UnsignedIntegerType>(_ v: T)

    /// Create an instance representing `v` in the given `radix` (base).
    ///
    /// Numerals greater than 9 are represented as roman letters,
    /// starting with `a` if `uppercase` is `false` or `A` otherwise.
    init<T : _SignedIntegerType>(_ v: T, radix: Int, uppercase: Bool = default)

    /// Create an instance representing `v` in the given `radix` (base).
    ///
    /// Numerals greater than 9 are represented as roman letters,
    /// starting with `a` if `uppercase` is `false` or `A` otherwise.
    init<T : _UnsignedIntegerType>(_ v: T, radix: Int, uppercase: Bool = default)
}





extension String {

    /// The value of `self` as a collection of [Unicode scalar values](http://www.unicode.org/glossary/#unicode_scalar_value).
    var unicodeScalars: String.UnicodeScalarView
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
    init<T>(_ instance: T)

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
    init<T>(reflecting subject: T)
}

extension String.CharacterView : CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType {

    /// A character position.
    struct Index : ForwardIndexType, _Incrementable, Equatable, BidirectionalIndexType, Comparable, Reflectable {

        /// Returns the next consecutive value after `self`.
        ///
        /// - Requires: The next value is representable.
        func successor() -> String.CharacterView.Index

        /// Returns the previous consecutive value before `self`.
        ///
        /// - Requires: The previous value is representable.
        func predecessor() -> String.CharacterView.Index

        /// Returns a mirror that reflects `self`.
        func getMirror() -> MirrorType
    }

    /// The position of the first `Character` if `self` is
    /// non-empty; identical to `endIndex` otherwise.
    var startIndex: String.CharacterView.Index { get }

    /// The "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    var endIndex: String.CharacterView.Index { get }
    subscript (i: String.CharacterView.Index) -> Character { get }

    /// Return a *generator* over the `Character`s.
    ///
    /// - Complexity: O(1).
    func generate() -> IndexingGenerator<String.CharacterView>
}

extension String.CharacterView : ExtensibleCollectionType {

    /// Create an empty instance.
    init()

    /// Reserve enough space to store `n` ASCII characters.
    ///
    /// - Complexity: O(`n`).
    mutating func reserveCapacity(n: Int)

    /// Append `c` to `self`.
    ///
    /// - Complexity: Amortized O(1).
    mutating func append(c: Character)

    /// Append the elements of `newElements` to `self`.
    mutating func extend<S : SequenceType where S.Generator.Element == Character>(newElements: S)

    /// Create an instance containing `characters`.
    init<S : SequenceType where S.Generator.Element == Character>(_ characters: S)
}

extension String.CharacterView {

    /// Interpose `self` between every pair of consecutive `elements`,
    /// then concatenate the result.  For example:
    ///
    ///     "-|-".join(["foo", "bar", "baz"]) // "foo-|-bar-|-baz"
    func join<S : SequenceType where S.Generator.Element == String.CharacterView>(elements: S) -> String.CharacterView
}

extension String.CharacterView : RangeReplaceableCollectionType {

    /// Replace the given `subRange` of elements with `newElements`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`subRange.count`) if `subRange.endIndex
    ///   == self.endIndex` and `isEmpty(newElements)`, O(N) otherwise.
    mutating func replaceRange<C : CollectionType where C.Generator.Element == Character>(subRange: Range<String.CharacterView.Index>, with newElements: C)

    /// Insert `newElement` at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count`).
    mutating func insert(newElement: Character, atIndex i: String.CharacterView.Index)

    /// Insert `newElements` at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count + newElements.count`).
    mutating func splice<S : CollectionType where S.Generator.Element == Character>(newElements: S, atIndex i: String.CharacterView.Index)

    /// Remove and return the element at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count`).
    mutating func removeAtIndex(i: String.CharacterView.Index) -> Character

    /// Remove the indicated `subRange` of characters.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count`).
    mutating func removeRange(subRange: Range<String.CharacterView.Index>)

    /// Remove all characters.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - parameter keepCapacity: If `true`, prevents the release of
    ///   allocated storage, which can be a useful optimization
    ///   when `self` is going to be grown again.
    mutating func removeAll(keepCapacity keepCapacity: Bool = default)
}

extension String.CharacterView : Sliceable, _Sliceable {
    subscript (subRange: Range<String.CharacterView.Index>) -> String.CharacterView { get }
}

extension String.UnicodeScalarView : ExtensibleCollectionType {

    /// Construct an empty instance.
    init()

    /// Reserve enough space to store `n` ASCII characters.
    ///
    /// - Complexity: O(`n`).
    mutating func reserveCapacity(n: Int)

    /// Append `x` to `self`.
    ///
    /// - Complexity: Amortized O(1).
    mutating func append(x: UnicodeScalar)

    /// Append the elements of `newElements` to `self`.
    ///
    /// - Complexity: O(*length of result*).
    mutating func extend<S : SequenceType where S.Generator.Element == UnicodeScalar>(newElements: S)
}

extension String.UnicodeScalarView : RangeReplaceableCollectionType {

    /// Replace the given `subRange` of elements with `newElements`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`subRange.count`) if `subRange.endIndex
    ///   == self.endIndex` and `isEmpty(newElements)`, O(N) otherwise.
    mutating func replaceRange<C : CollectionType where C.Generator.Element == UnicodeScalar>(subRange: Range<String.UnicodeScalarView.Index>, with newElements: C)

    /// Insert `newElement` at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count`).
    mutating func insert(newElement: UnicodeScalar, atIndex i: String.UnicodeScalarView.Index)

    /// Insert `newElements` at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count + newElements.count`).
    mutating func splice<S : CollectionType where S.Generator.Element == UnicodeScalar>(newElements: S, atIndex i: String.UnicodeScalarView.Index)

    /// Remove the and return element at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count`).
    mutating func removeAtIndex(i: String.UnicodeScalarView.Index) -> UnicodeScalar

    /// Remove the indicated `subRange` of elements.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count`).
    mutating func removeRange(subRange: Range<String.UnicodeScalarView.Index>)

    /// Remove all elements.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - parameter keepCapacity: If `true`, prevents the release of
    ///   allocated storage, which can be a useful optimization
    ///   when `self` is going to be grown again.
    mutating func removeAll(keepCapacity keepCapacity: Bool = default)
}

extension String.CharacterView.Index {

    /// Construct the position in `characters` that corresponds exactly to
    /// `unicodeScalarIndex`. If no such position exists, the result is `nil`.
    ///
    /// - Requires: `unicodeScalarIndex` is an element of
    ///   `characters.unicodeScalars.indices`.
    init?(_ unicodeScalarIndex: UnicodeScalarIndex, within characters: String)

    /// Construct the position in `characters` that corresponds exactly to
    /// `utf16Index`. If no such position exists, the result is `nil`.
    ///
    /// - Requires: `utf16Index` is an element of
    ///   `characters.utf16.indices`.
    init?(_ utf16Index: UTF16Index, within characters: String)

    /// Construct the position in `characters` that corresponds exactly to
    /// `utf8Index`. If no such position exists, the result is `nil`.
    ///
    /// - Requires: `utf8Index` is an element of
    ///   `characters.utf8.indices`.
    init?(_ utf8Index: UTF8Index, within characters: String)

    /// Return the position in `utf8` that corresponds exactly
    /// to `self`.
    ///
    /// - Requires: `self` is an element of `String(utf8).indices`.
    func samePositionIn(utf8: String.UTF8View) -> String.UTF8View.Index

    /// Return the position in `utf16` that corresponds exactly
    /// to `self`.
    ///
    /// - Requires: `self` is an element of `String(utf16).indices`.
    func samePositionIn(utf16: String.UTF16View) -> String.UTF16View.Index

    /// Return the position in `unicodeScalars` that corresponds exactly
    /// to `self`.
    ///
    /// - Requires: `self` is an element of `String(unicodeScalars).indices`.
    func samePositionIn(unicodeScalars: String.UnicodeScalarView) -> String.UnicodeScalarView.Index
}

extension String.UnicodeScalarView.Index {

    /// Construct the position in `unicodeScalars` that corresponds exactly to
    /// `utf16Index`. If no such position exists, the result is `nil`.
    ///
    /// - Requires: `utf16Index` is an element of
    ///   `String(unicodeScalars).utf16.indices`.
    init?(_ utf16Index: UTF16Index, within unicodeScalars: String.UnicodeScalarView)

    /// Construct the position in `unicodeScalars` that corresponds exactly to
    /// `utf8Index`. If no such position exists, the result is `nil`.
    ///
    /// - Requires: `utf8Index` is an element of
    ///   `String(unicodeScalars).utf8.indices`.
    init?(_ utf8Index: UTF8Index, within unicodeScalars: String.UnicodeScalarView)

    /// Construct the position in `unicodeScalars` that corresponds
    /// exactly to `characterIndex`.
    ///
    /// - Requires: `characterIndex` is an element of
    ///   `String(unicodeScalars).indices`.
    init(_ characterIndex: Index, within unicodeScalars: String.UnicodeScalarView)

    /// Return the position in `utf8` that corresponds exactly
    /// to `self`.
    ///
    /// - Requires: `self` is an element of `String(utf8)!.indices`.
    func samePositionIn(utf8: String.UTF8View) -> String.UTF8View.Index

    /// Return the position in `utf16` that corresponds exactly
    /// to `self`.
    ///
    /// - Requires: `self` is an element of `String(utf16)!.indices`.
    func samePositionIn(utf16: String.UTF16View) -> String.UTF16View.Index

    /// Return the position in `characters` that corresponds exactly
    /// to `self`, or if no such position exists, `nil`.
    ///
    /// - Requires: `self` is an element of `characters.unicodeScalars.indices`.
    func samePositionIn(characters: String) -> Index?
}

extension String.UTF16View.Index : ForwardIndexType, _Incrementable, BidirectionalIndexType {
    typealias Distance = Int
    func successor() -> String.UTF16View.Index
    func predecessor() -> String.UTF16View.Index
}

extension String.UTF16View.Index : Equatable, Comparable {
}

extension String.UTF16View.Index {

    /// Construct the position in `utf16` that corresponds exactly to
    /// `utf8Index`. If no such position exists, the result is `nil`.
    ///
    /// - Requires: `utf8Index` is an element of
    ///   `String(utf16)!.utf8.indices`.
    init?(_ utf8Index: UTF8Index, within utf16: String.UTF16View)

    /// Construct the position in `utf16` that corresponds exactly to
    /// `unicodeScalarIndex`.
    ///
    /// - Requires: `unicodeScalarIndex` is an element of
    ///   `String(utf16)!.unicodeScalars.indices`.
    init(_ unicodeScalarIndex: UnicodeScalarIndex, within utf16: String.UTF16View)

    /// Construct the position in `utf16` that corresponds exactly to
    /// `characterIndex`.
    ///
    /// - Requires: `characterIndex` is an element of
    ///   `String(utf16)!.indices`.
    init(_ characterIndex: Index, within utf16: String.UTF16View)

    /// Return the position in `utf8` that corresponds exactly
    /// to `self`, or if no such position exists, `nil`.
    ///
    /// - Requires: `self` is an element of
    ///   `String(utf8)!.utf16.indices`.
    func samePositionIn(utf8: String.UTF8View) -> String.UTF8View.Index?

    /// Return the position in `unicodeScalars` that corresponds exactly
    /// to `self`, or if no such position exists, `nil`.
    ///
    /// - Requires: `self` is an element of
    ///   `String(unicodeScalars).utf16.indices`.
    func samePositionIn(unicodeScalars: String.UnicodeScalarView) -> UnicodeScalarIndex?

    /// Return the position in `characters` that corresponds exactly
    /// to `self`, or if no such position exists, `nil`.
    ///
    /// - Requires: `self` is an element of `characters.utf16.indices`.
    func samePositionIn(characters: String) -> Index?
}

extension String.UTF8View.Index {

    /// Construct the position in `utf8` that corresponds exactly to
    /// `utf16Index`. If no such position exists, the result is `nil`.
    ///
    /// - Requires: `utf8Index` is an element of
    ///   `String(utf16)!.utf8.indices`.
    init?(_ utf16Index: UTF16Index, within utf8: String.UTF8View)

    /// Construct the position in `utf8` that corresponds exactly to
    /// `unicodeScalarIndex`.
    ///
    /// - Requires: `unicodeScalarIndex` is an element of
    ///   `String(utf8)!.unicodeScalars.indices`.
    init(_ unicodeScalarIndex: UnicodeScalarIndex, within utf8: String.UTF8View)

    /// Construct the position in `utf8` that corresponds exactly to
    /// `characterIndex`.
    ///
    /// - Requires: `characterIndex` is an element of
    ///   `String(utf8)!.indices`.
    init(_ characterIndex: Index, within utf8: String.UTF8View)

    /// Return the position in `utf16` that corresponds exactly
    /// to `self`, or if no such position exists, `nil`.
    ///
    /// - Requires: `self` is an element of `String(utf16)!.utf8.indices`.
    func samePositionIn(utf16: String.UTF16View) -> String.UTF16View.Index?

    /// Return the position in `unicodeScalars` that corresponds exactly
    /// to `self`, or if no such position exists, `nil`.
    ///
    /// - Requires: `self` is an element of
    ///   `String(unicodeScalars).utf8.indices`.
    func samePositionIn(unicodeScalars: String.UnicodeScalarView) -> UnicodeScalarIndex?

    /// Return the position in `characters` that corresponds exactly
    /// to `self`, or if no such position exists, `nil`.
    ///
    /// - Requires: `self` is an element of `characters.utf8.indices`.
    func samePositionIn(characters: String) -> Index?
}


/// Conforming types can be initialized with string interpolations
/// containing `\(`...`)` clauses.
protocol StringInterpolationConvertible {

    /// Create an instance by concatenating the elements of `strings`.
    init(stringInterpolation strings: Self...)

    /// Create an instance containing `expr`'s `print` representation.
    init<T>(stringInterpolationSegment expr: T)
}


/// Conforming types can be initialized with arbitrary string literals.
protocol StringLiteralConvertible : ExtendedGraphemeClusterLiteralConvertible {
    typealias StringLiteralType

    /// Create an instance initialized to `value`.
    init(stringLiteral value: Self.StringLiteralType)
}


/// The default type for an otherwise-unconstrained string literal.
typealias StringLiteralType = String


/// A 64-bit unsigned integer value
/// type.
struct UInt : UnsignedIntegerType, IntegerType, Equatable, Comparable, _IntegerType, IntegerArithmeticType, _IntegerArithmeticType, IntegerLiteralConvertible, _UnsignedIntegerType {
    var value: Builtin.Int64

    /// A type that can represent the number of steps between pairs of
    /// values.
    typealias Distance = Int

    /// Create an instance initialized to zero.
    init()
    init(_ v: Builtin.Word)

    /// Create an instance initialized to `value`.
    init(_ value: UInt)

    /// Creates an integer from its big-endian representation, changing the
    /// byte order if necessary.
    init(bigEndian value: UInt)

    /// Creates an integer from its little-endian representation, changing the
    /// byte order if necessary.
    init(littleEndian value: UInt)
    init(_builtinIntegerLiteral value: Builtin.Int2048)

    /// Create an instance initialized to `value`.
    init(integerLiteral value: UInt)

    /// Returns the big-endian representation of the integer, changing the
    /// byte order if necessary.
    var bigEndian: UInt { get }

    /// Returns the little-endian representation of the integer, changing the
    /// byte order if necessary.
    var littleEndian: UInt { get }

    /// Returns the current integer with the byte order swapped.
    var byteSwapped: UInt { get }
    static var max: UInt { get }
    static var min: UInt { get }
}

extension UInt : Hashable {

    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    var hashValue: Int { get }
}

extension UInt : CustomStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }
}

extension UInt : RandomAccessIndexType, BidirectionalIndexType, ForwardIndexType, _Incrementable, Strideable, _Strideable {

    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    func successor() -> UInt

    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    func predecessor() -> UInt

    /// Return the minimum number of applications of `successor` or
    /// `predecessor` required to reach `other` from `self`.
    ///
    /// - Complexity: O(1).
    func distanceTo(other: UInt) -> Distance

    /// Return `self` offset by `n` steps.
    ///
    /// - Returns: If `n > 0`, the result of applying `successor` to
    ///   `self` `n` times.  If `n < 0`, the result of applying
    ///   `predecessor` to `self` `-n` times. Otherwise, `self`.
    ///
    /// - Complexity: O(1).
    func advancedBy(amount: Distance) -> UInt
}

extension UInt {

    /// Add `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func addWithOverflow(lhs: UInt, _ rhs: UInt) -> (UInt, overflow: Bool)

    /// Subtract `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func subtractWithOverflow(lhs: UInt, _ rhs: UInt) -> (UInt, overflow: Bool)

    /// Multiply `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func multiplyWithOverflow(lhs: UInt, _ rhs: UInt) -> (UInt, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning
    /// a result and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    static func divideWithOverflow(lhs: UInt, _ rhs: UInt) -> (UInt, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning
    /// the remainder and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    static func remainderWithOverflow(lhs: UInt, _ rhs: UInt) -> (UInt, overflow: Bool)

    /// Represent this number using Swift's widest native unsigned
    /// integer type.
    func toUIntMax() -> UIntMax

    /// Explicitly convert to `IntMax`, trapping on overflow (except in -Ounchecked builds).
    func toIntMax() -> IntMax
}

extension UInt {
    init(_ v: UInt8)
    init(_ v: Int8)
    init(_ v: UInt16)
    init(_ v: Int16)
    init(_ v: UInt32)
    init(_ v: Int32)
    init(_ v: UInt64)

    /// Construct a `UInt` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: UInt64)
    init(_ v: Int64)

    /// Construct a `UInt` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: Int64)
    init(_ v: Int)

    /// Construct a `UInt` having the same memory representation as
    /// the `Int` `bitPattern`.  No range or overflow checking
    /// occurs, and the resulting `UInt` may not have the same numeric
    /// value as `bitPattern`--it is only guaranteed to use the same
    /// pattern of bits.
    init(bitPattern: Int)
}

extension UInt : BitwiseOperationsType {

    /// The empty bitset of type UInt.
    static var allZeros: UInt { get }
}

extension UInt {

    /// Construct an instance that approximates `other`.
    init(_ other: Float)

    /// Construct an instance that approximates `other`.
    init(_ other: Double)

    /// Construct an instance that approximates `other`.
    init(_ other: Float80)
}

extension UInt {

    /// Construct from an ASCII representation in the given `radix`.
    ///
    /// If `text` does not match the regular expression
    /// "[+-][0-9a-zA-Z]+", or the value it denotes in the given `radix`
    /// is not representable, the result is `nil`.
    init?(_ text: String, radix: Int = default)
}

extension UInt : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension UInt : CVarArgType {
}


/// A 16-bit unsigned integer value
/// type.
struct UInt16 : UnsignedIntegerType, IntegerType, Equatable, Comparable, _IntegerType, IntegerArithmeticType, _IntegerArithmeticType, IntegerLiteralConvertible, _UnsignedIntegerType {
    var value: Builtin.Int16

    /// A type that can represent the number of steps between pairs of
    /// values.
    typealias Distance = Int

    /// Create an instance initialized to zero.
    init()

    /// Create an instance initialized to `value`.
    init(_ value: UInt16)

    /// Creates an integer from its big-endian representation, changing the
    /// byte order if necessary.
    init(bigEndian value: UInt16)

    /// Creates an integer from its little-endian representation, changing the
    /// byte order if necessary.
    init(littleEndian value: UInt16)
    init(_builtinIntegerLiteral value: Builtin.Int2048)

    /// Create an instance initialized to `value`.
    init(integerLiteral value: UInt16)

    /// Returns the big-endian representation of the integer, changing the
    /// byte order if necessary.
    var bigEndian: UInt16 { get }

    /// Returns the little-endian representation of the integer, changing the
    /// byte order if necessary.
    var littleEndian: UInt16 { get }

    /// Returns the current integer with the byte order swapped.
    var byteSwapped: UInt16 { get }
    static var max: UInt16 { get }
    static var min: UInt16 { get }
}

extension UInt16 : Hashable {

    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    var hashValue: Int { get }
}

extension UInt16 : CustomStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }
}

extension UInt16 : RandomAccessIndexType, BidirectionalIndexType, ForwardIndexType, _Incrementable, Strideable, _Strideable {

    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    func successor() -> UInt16

    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    func predecessor() -> UInt16

    /// Return the minimum number of applications of `successor` or
    /// `predecessor` required to reach `other` from `self`.
    ///
    /// - Complexity: O(1).
    func distanceTo(other: UInt16) -> Distance

    /// Return `self` offset by `n` steps.
    ///
    /// - Returns: If `n > 0`, the result of applying `successor` to
    ///   `self` `n` times.  If `n < 0`, the result of applying
    ///   `predecessor` to `self` `-n` times. Otherwise, `self`.
    ///
    /// - Complexity: O(1).
    func advancedBy(amount: Distance) -> UInt16
}

extension UInt16 {

    /// Add `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func addWithOverflow(lhs: UInt16, _ rhs: UInt16) -> (UInt16, overflow: Bool)

    /// Subtract `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func subtractWithOverflow(lhs: UInt16, _ rhs: UInt16) -> (UInt16, overflow: Bool)

    /// Multiply `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func multiplyWithOverflow(lhs: UInt16, _ rhs: UInt16) -> (UInt16, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning
    /// a result and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    static func divideWithOverflow(lhs: UInt16, _ rhs: UInt16) -> (UInt16, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning
    /// the remainder and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    static func remainderWithOverflow(lhs: UInt16, _ rhs: UInt16) -> (UInt16, overflow: Bool)

    /// Represent this number using Swift's widest native unsigned
    /// integer type.
    func toUIntMax() -> UIntMax

    /// Explicitly convert to `IntMax`.
    func toIntMax() -> IntMax
}

extension UInt16 {
    init(_ v: UInt8)
    init(_ v: Int8)
    init(_ v: Int16)
    init(_ v: UInt32)

    /// Construct a `UInt16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: UInt32)
    init(_ v: Int32)

    /// Construct a `UInt16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: Int32)
    init(_ v: UInt64)

    /// Construct a `UInt16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: UInt64)
    init(_ v: Int64)

    /// Construct a `UInt16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: Int64)
    init(_ v: UInt)

    /// Construct a `UInt16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: UInt)
    init(_ v: Int)

    /// Construct a `UInt16` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: Int)

    /// Construct a `UInt16` having the same memory representation as
    /// the `Int16` `bitPattern`.  No range or overflow checking
    /// occurs, and the resulting `UInt16` may not have the same numeric
    /// value as `bitPattern`--it is only guaranteed to use the same
    /// pattern of bits.
    init(bitPattern: Int16)
}

extension UInt16 : BitwiseOperationsType {

    /// The empty bitset of type UInt16.
    static var allZeros: UInt16 { get }
}

extension UInt16 {

    /// Construct an instance that approximates `other`.
    init(_ other: Float)

    /// Construct an instance that approximates `other`.
    init(_ other: Double)

    /// Construct an instance that approximates `other`.
    init(_ other: Float80)
}

extension UInt16 {

    /// Construct from an ASCII representation in the given `radix`.
    ///
    /// If `text` does not match the regular expression
    /// "[+-][0-9a-zA-Z]+", or the value it denotes in the given `radix`
    /// is not representable, the result is `nil`.
    init?(_ text: String, radix: Int = default)
}

extension UInt16 : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension UInt16 : _StringElementType {
}

extension UInt16 : CVarArgType {
}


/// A 32-bit unsigned integer value
/// type.
struct UInt32 : UnsignedIntegerType, IntegerType, Equatable, Comparable, _IntegerType, IntegerArithmeticType, _IntegerArithmeticType, IntegerLiteralConvertible, _UnsignedIntegerType {
    var value: Builtin.Int32

    /// A type that can represent the number of steps between pairs of
    /// values.
    typealias Distance = Int

    /// Create an instance initialized to zero.
    init()

    /// Create an instance initialized to `value`.
    init(_ value: UInt32)

    /// Creates an integer from its big-endian representation, changing the
    /// byte order if necessary.
    init(bigEndian value: UInt32)

    /// Creates an integer from its little-endian representation, changing the
    /// byte order if necessary.
    init(littleEndian value: UInt32)
    init(_builtinIntegerLiteral value: Builtin.Int2048)

    /// Create an instance initialized to `value`.
    init(integerLiteral value: UInt32)

    /// Returns the big-endian representation of the integer, changing the
    /// byte order if necessary.
    var bigEndian: UInt32 { get }

    /// Returns the little-endian representation of the integer, changing the
    /// byte order if necessary.
    var littleEndian: UInt32 { get }

    /// Returns the current integer with the byte order swapped.
    var byteSwapped: UInt32 { get }
    static var max: UInt32 { get }
    static var min: UInt32 { get }
}

extension UInt32 : Hashable {

    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    var hashValue: Int { get }
}

extension UInt32 : CustomStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }
}

extension UInt32 : RandomAccessIndexType, BidirectionalIndexType, ForwardIndexType, _Incrementable, Strideable, _Strideable {

    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    func successor() -> UInt32

    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    func predecessor() -> UInt32

    /// Return the minimum number of applications of `successor` or
    /// `predecessor` required to reach `other` from `self`.
    ///
    /// - Complexity: O(1).
    func distanceTo(other: UInt32) -> Distance

    /// Return `self` offset by `n` steps.
    ///
    /// - Returns: If `n > 0`, the result of applying `successor` to
    ///   `self` `n` times.  If `n < 0`, the result of applying
    ///   `predecessor` to `self` `-n` times. Otherwise, `self`.
    ///
    /// - Complexity: O(1).
    func advancedBy(amount: Distance) -> UInt32
}

extension UInt32 {

    /// Add `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func addWithOverflow(lhs: UInt32, _ rhs: UInt32) -> (UInt32, overflow: Bool)

    /// Subtract `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func subtractWithOverflow(lhs: UInt32, _ rhs: UInt32) -> (UInt32, overflow: Bool)

    /// Multiply `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func multiplyWithOverflow(lhs: UInt32, _ rhs: UInt32) -> (UInt32, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning
    /// a result and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    static func divideWithOverflow(lhs: UInt32, _ rhs: UInt32) -> (UInt32, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning
    /// the remainder and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    static func remainderWithOverflow(lhs: UInt32, _ rhs: UInt32) -> (UInt32, overflow: Bool)

    /// Represent this number using Swift's widest native unsigned
    /// integer type.
    func toUIntMax() -> UIntMax

    /// Explicitly convert to `IntMax`.
    func toIntMax() -> IntMax
}

extension UInt32 {
    init(_ v: UInt8)
    init(_ v: Int8)
    init(_ v: UInt16)
    init(_ v: Int16)
    init(_ v: Int32)
    init(_ v: UInt64)

    /// Construct a `UInt32` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: UInt64)
    init(_ v: Int64)

    /// Construct a `UInt32` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: Int64)
    init(_ v: UInt)

    /// Construct a `UInt32` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: UInt)
    init(_ v: Int)

    /// Construct a `UInt32` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: Int)

    /// Construct a `UInt32` having the same memory representation as
    /// the `Int32` `bitPattern`.  No range or overflow checking
    /// occurs, and the resulting `UInt32` may not have the same numeric
    /// value as `bitPattern`--it is only guaranteed to use the same
    /// pattern of bits.
    init(bitPattern: Int32)
}

extension UInt32 : BitwiseOperationsType {

    /// The empty bitset of type UInt32.
    static var allZeros: UInt32 { get }
}

extension UInt32 {

    /// Construct an instance that approximates `other`.
    init(_ other: Float)

    /// Construct an instance that approximates `other`.
    init(_ other: Double)

    /// Construct an instance that approximates `other`.
    init(_ other: Float80)
}

extension UInt32 {

    /// Construct from an ASCII representation in the given `radix`.
    ///
    /// If `text` does not match the regular expression
    /// "[+-][0-9a-zA-Z]+", or the value it denotes in the given `radix`
    /// is not representable, the result is `nil`.
    init?(_ text: String, radix: Int = default)
}

extension UInt32 : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension UInt32 {

    /// Construct with value `v.value`.
    ///
    /// - Requires: `v.value` can be represented as UInt32.
    init(_ v: UnicodeScalar)
}

extension UInt32 : CVarArgType {
}


/// A 64-bit unsigned integer value
/// type.
struct UInt64 : UnsignedIntegerType, IntegerType, Equatable, Comparable, _IntegerType, IntegerArithmeticType, _IntegerArithmeticType, IntegerLiteralConvertible, _UnsignedIntegerType {
    var value: Builtin.Int64

    /// A type that can represent the number of steps between pairs of
    /// values.
    typealias Distance = Int

    /// Create an instance initialized to zero.
    init()

    /// Create an instance initialized to `value`.
    init(_ value: UInt64)

    /// Creates an integer from its big-endian representation, changing the
    /// byte order if necessary.
    init(bigEndian value: UInt64)

    /// Creates an integer from its little-endian representation, changing the
    /// byte order if necessary.
    init(littleEndian value: UInt64)
    init(_builtinIntegerLiteral value: Builtin.Int2048)

    /// Create an instance initialized to `value`.
    init(integerLiteral value: UInt64)

    /// Returns the big-endian representation of the integer, changing the
    /// byte order if necessary.
    var bigEndian: UInt64 { get }

    /// Returns the little-endian representation of the integer, changing the
    /// byte order if necessary.
    var littleEndian: UInt64 { get }

    /// Returns the current integer with the byte order swapped.
    var byteSwapped: UInt64 { get }
    static var max: UInt64 { get }
    static var min: UInt64 { get }
}

extension UInt64 : Hashable {

    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    var hashValue: Int { get }
}

extension UInt64 : CustomStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }
}

extension UInt64 : RandomAccessIndexType, BidirectionalIndexType, ForwardIndexType, _Incrementable, Strideable, _Strideable {

    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    func successor() -> UInt64

    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    func predecessor() -> UInt64

    /// Return the minimum number of applications of `successor` or
    /// `predecessor` required to reach `other` from `self`.
    ///
    /// - Complexity: O(1).
    func distanceTo(other: UInt64) -> Distance

    /// Return `self` offset by `n` steps.
    ///
    /// - Returns: If `n > 0`, the result of applying `successor` to
    ///   `self` `n` times.  If `n < 0`, the result of applying
    ///   `predecessor` to `self` `-n` times. Otherwise, `self`.
    ///
    /// - Complexity: O(1).
    func advancedBy(amount: Distance) -> UInt64
}

extension UInt64 {

    /// Add `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func addWithOverflow(lhs: UInt64, _ rhs: UInt64) -> (UInt64, overflow: Bool)

    /// Subtract `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func subtractWithOverflow(lhs: UInt64, _ rhs: UInt64) -> (UInt64, overflow: Bool)

    /// Multiply `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func multiplyWithOverflow(lhs: UInt64, _ rhs: UInt64) -> (UInt64, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning
    /// a result and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    static func divideWithOverflow(lhs: UInt64, _ rhs: UInt64) -> (UInt64, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning
    /// the remainder and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    static func remainderWithOverflow(lhs: UInt64, _ rhs: UInt64) -> (UInt64, overflow: Bool)

    /// Represent this number using Swift's widest native unsigned
    /// integer type.
    func toUIntMax() -> UIntMax

    /// Explicitly convert to `IntMax`, trapping on overflow (except in -Ounchecked builds).
    func toIntMax() -> IntMax
}

extension UInt64 {
    init(_ v: UInt8)
    init(_ v: Int8)
    init(_ v: UInt16)
    init(_ v: Int16)
    init(_ v: UInt32)
    init(_ v: Int32)
    init(_ v: Int64)
    init(_ v: UInt)
    init(_ v: Int)

    /// Construct a `UInt64` having the same memory representation as
    /// the `Int64` `bitPattern`.  No range or overflow checking
    /// occurs, and the resulting `UInt64` may not have the same numeric
    /// value as `bitPattern`--it is only guaranteed to use the same
    /// pattern of bits.
    init(bitPattern: Int64)
}

extension UInt64 : BitwiseOperationsType {

    /// The empty bitset of type UInt64.
    static var allZeros: UInt64 { get }
}

extension UInt64 {

    /// Construct an instance that approximates `other`.
    init(_ other: Float)

    /// Construct an instance that approximates `other`.
    init(_ other: Double)

    /// Construct an instance that approximates `other`.
    init(_ other: Float80)
}

extension UInt64 {

    /// Construct from an ASCII representation in the given `radix`.
    ///
    /// If `text` does not match the regular expression
    /// "[+-][0-9a-zA-Z]+", or the value it denotes in the given `radix`
    /// is not representable, the result is `nil`.
    init?(_ text: String, radix: Int = default)
}

extension UInt64 : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension UInt64 {

    /// Construct with value `v.value`.
    ///
    /// - Requires: `v.value` can be represented as UInt64.
    init(_ v: UnicodeScalar)
}

extension UInt64 : CVarArgType, _CVarArgAlignedType {
}


/// A 8-bit unsigned integer value
/// type.
struct UInt8 : UnsignedIntegerType, IntegerType, Equatable, Comparable, _IntegerType, IntegerArithmeticType, _IntegerArithmeticType, IntegerLiteralConvertible, _UnsignedIntegerType {
    var value: Builtin.Int8

    /// A type that can represent the number of steps between pairs of
    /// values.
    typealias Distance = Int

    /// Create an instance initialized to zero.
    init()

    /// Create an instance initialized to `value`.
    init(_ value: UInt8)
    init(_builtinIntegerLiteral value: Builtin.Int2048)

    /// Create an instance initialized to `value`.
    init(integerLiteral value: UInt8)
    static var max: UInt8 { get }
    static var min: UInt8 { get }
}

extension UInt8 : Hashable {

    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    var hashValue: Int { get }
}

extension UInt8 : CustomStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }
}

extension UInt8 : RandomAccessIndexType, BidirectionalIndexType, ForwardIndexType, _Incrementable, Strideable, _Strideable {

    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    func successor() -> UInt8

    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    func predecessor() -> UInt8

    /// Return the minimum number of applications of `successor` or
    /// `predecessor` required to reach `other` from `self`.
    ///
    /// - Complexity: O(1).
    func distanceTo(other: UInt8) -> Distance

    /// Return `self` offset by `n` steps.
    ///
    /// - Returns: If `n > 0`, the result of applying `successor` to
    ///   `self` `n` times.  If `n < 0`, the result of applying
    ///   `predecessor` to `self` `-n` times. Otherwise, `self`.
    ///
    /// - Complexity: O(1).
    func advancedBy(amount: Distance) -> UInt8
}

extension UInt8 {

    /// Add `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func addWithOverflow(lhs: UInt8, _ rhs: UInt8) -> (UInt8, overflow: Bool)

    /// Subtract `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func subtractWithOverflow(lhs: UInt8, _ rhs: UInt8) -> (UInt8, overflow: Bool)

    /// Multiply `lhs` and `rhs`, returning a result and a
    /// `Bool` that is true iff the operation caused an arithmetic
    /// overflow.
    static func multiplyWithOverflow(lhs: UInt8, _ rhs: UInt8) -> (UInt8, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning
    /// a result and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    static func divideWithOverflow(lhs: UInt8, _ rhs: UInt8) -> (UInt8, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning
    /// the remainder and a `Bool`
    /// that is true iff the operation caused an arithmetic overflow.
    static func remainderWithOverflow(lhs: UInt8, _ rhs: UInt8) -> (UInt8, overflow: Bool)

    /// Represent this number using Swift's widest native unsigned
    /// integer type.
    func toUIntMax() -> UIntMax

    /// Explicitly convert to `IntMax`.
    func toIntMax() -> IntMax
}

extension UInt8 {
    init(_ v: Int8)
    init(_ v: UInt16)

    /// Construct a `UInt8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: UInt16)
    init(_ v: Int16)

    /// Construct a `UInt8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: Int16)
    init(_ v: UInt32)

    /// Construct a `UInt8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: UInt32)
    init(_ v: Int32)

    /// Construct a `UInt8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: Int32)
    init(_ v: UInt64)

    /// Construct a `UInt8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: UInt64)
    init(_ v: Int64)

    /// Construct a `UInt8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: Int64)
    init(_ v: UInt)

    /// Construct a `UInt8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: UInt)
    init(_ v: Int)

    /// Construct a `UInt8` having the same bitwise representation as
    /// the least significant bits of the provided bit pattern.
    ///
    /// No range or overflow checking occurs.
    init(truncatingBitPattern: Int)

    /// Construct a `UInt8` having the same memory representation as
    /// the `Int8` `bitPattern`.  No range or overflow checking
    /// occurs, and the resulting `UInt8` may not have the same numeric
    /// value as `bitPattern`--it is only guaranteed to use the same
    /// pattern of bits.
    init(bitPattern: Int8)
}

extension UInt8 : BitwiseOperationsType {

    /// The empty bitset of type UInt8.
    static var allZeros: UInt8 { get }
}

extension UInt8 {

    /// Construct an instance that approximates `other`.
    init(_ other: Float)

    /// Construct an instance that approximates `other`.
    init(_ other: Double)

    /// Construct an instance that approximates `other`.
    init(_ other: Float80)
}

extension UInt8 {

    /// Construct from an ASCII representation in the given `radix`.
    ///
    /// If `text` does not match the regular expression
    /// "[+-][0-9a-zA-Z]+", or the value it denotes in the given `radix`
    /// is not representable, the result is `nil`.
    init?(_ text: String, radix: Int = default)
}

extension UInt8 : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension UInt8 : _StringElementType {
}

extension UInt8 {

    /// Construct with value `v.value`.
    ///
    /// - Requires: `v.value` can be represented as ASCII (0..<128).
    init(ascii v: UnicodeScalar)
}

extension UInt8 : CVarArgType {
}


/// The largest native unsigned integer type.
typealias UIntMax = UInt64


/// A codec for [UTF-16](http://www.unicode.org/glossary/#UTF_16).
struct UTF16 : UnicodeCodecType {

    /// A type that can hold [code unit](http://www.unicode.org/glossary/#code_unit) values for this
    /// encoding.
    typealias CodeUnit = UInt16
    init()

    /// Start or continue decoding a UTF sequence.
    ///
    /// In order to decode a code unit sequence completely, this function should
    /// be called repeatedly until it returns `UnicodeDecodingResult.EmptyInput`.
    /// Checking that the generator was exhausted is not sufficient.  The decoder
    /// can have an internal buffer that is pre-filled with data from the input
    /// generator.
    ///
    /// Because of buffering, it is impossible to find the corresponing position
    /// in the generator for a given returned `UnicodeScalar` or an error.
    ///
    /// - parameter next: A *generator* of code units to be decoded.
    mutating func decode<G : GeneratorType where G.Element == CodeUnit>(inout input: G) -> UnicodeDecodingResult

    /// Encode a `UnicodeScalar` as a series of `CodeUnit`s by
    /// `put`'ing each `CodeUnit` to `output`.
    static func encode<S : SinkType where S.Element == CodeUnit>(input: UnicodeScalar, inout output: S)
}

extension UTF16 {

    /// Return the number of code units required to encode `x`.
    static func width(x: UnicodeScalar) -> Int

    /// Return the high surrogate code unit of a [surrogate pair](http://www.unicode.org/glossary/#surrogate_pair) representing
    /// `x`.
    ///
    /// - Requires: `width(x) == 2`.
    static func leadSurrogate(x: UnicodeScalar) -> CodeUnit

    /// Return the low surrogate code unit of a [surrogate pair](http://www.unicode.org/glossary/#surrogate_pair) representing
    /// `x`.
    ///
    /// - Requires: `width(x) == 2`.
    static func trailSurrogate(x: UnicodeScalar) -> CodeUnit
    static func isLeadSurrogate(x: CodeUnit) -> Bool
    static func isTrailSurrogate(x: CodeUnit) -> Bool

    /// Returns the number of UTF-16 code units required for the given code unit
    /// sequence when transcoded to UTF-16, and a bit describing if the sequence
    /// was found to contain only ASCII characters.
    ///
    /// If `repairIllFormedSequences` is `true`, the function always succeeds.
    /// If it is `false`, `nil` is returned if an ill-formed code unit sequence is
    /// found in `input`.
    static func measure<Encoding : UnicodeCodecType, Input : GeneratorType where Encoding.CodeUnit == Input.Element>(_: Encoding.Type, input: Input, repairIllFormedSequences: Bool) -> (Int, Bool)?
}


/// A codec for [UTF-32](http://www.unicode.org/glossary/#UTF_32).
struct UTF32 : UnicodeCodecType {

    /// A type that can hold [code unit](http://www.unicode.org/glossary/#code_unit) values for this
    /// encoding.
    typealias CodeUnit = UInt32
    init()

    /// Start or continue decoding a UTF sequence.
    ///
    /// In order to decode a code unit sequence completely, this function should
    /// be called repeatedly until it returns `UnicodeDecodingResult.EmptyInput`.
    /// Checking that the generator was exhausted is not sufficient.  The decoder
    /// can have an internal buffer that is pre-filled with data from the input
    /// generator.
    ///
    /// Because of buffering, it is impossible to find the corresponing position
    /// in the generator for a given returned `UnicodeScalar` or an error.
    ///
    /// - parameter next: A *generator* of code units to be decoded.
    mutating func decode<G : GeneratorType where G.Element == CodeUnit>(inout input: G) -> UnicodeDecodingResult

    /// Encode a `UnicodeScalar` as a series of `CodeUnit`s by
    /// `put`'ing each `CodeUnit` to `output`.
    static func encode<S : SinkType where S.Element == CodeUnit>(input: UnicodeScalar, inout output: S)
}


/// A codec for [UTF-8](http://www.unicode.org/glossary/#UTF_8).
struct UTF8 : UnicodeCodecType {

    /// A type that can hold [code unit](http://www.unicode.org/glossary/#code_unit) values for this
    /// encoding.
    typealias CodeUnit = UInt8
    init()

    /// Start or continue decoding a UTF sequence.
    ///
    /// In order to decode a code unit sequence completely, this function should
    /// be called repeatedly until it returns `UnicodeDecodingResult.EmptyInput`.
    /// Checking that the generator was exhausted is not sufficient.  The decoder
    /// can have an internal buffer that is pre-filled with data from the input
    /// generator.
    ///
    /// Because of buffering, it is impossible to find the corresponing position
    /// in the generator for a given returned `UnicodeScalar` or an error.
    ///
    /// - parameter next: A *generator* of code units to be decoded.
    mutating func decode<G : GeneratorType where G.Element == CodeUnit>(inout next: G) -> UnicodeDecodingResult

    /// Encode a `UnicodeScalar` as a series of `CodeUnit`s by
    /// `put`'ing each `CodeUnit` to `output`.
    static func encode<S : SinkType where S.Element == CodeUnit>(input: UnicodeScalar, inout output: S)

    /// Return `true` if `byte` is a continuation byte of the form
    /// `0b10xxxxxx`.
    static func isContinuation(byte: CodeUnit) -> Bool
}


/// An unsigned integer type that occupies one machine word.
typealias UWord = UInt


/// A Unicode [encoding scheme](http://www.unicode.org/glossary/#character_encoding_scheme).
///
/// Consists of an underlying [code unit](http://www.unicode.org/glossary/#code_unit) and functions to
/// translate between sequences of these code units and [unicode scalar values](http://www.unicode.org/glossary/#unicode_scalar_value).
protocol UnicodeCodecType {

    /// A type that can hold [code unit](http://www.unicode.org/glossary/#code_unit) values for this
    /// encoding.
    typealias CodeUnit
    init()

    /// Start or continue decoding a UTF sequence.
    ///
    /// In order to decode a code unit sequence completely, this function should
    /// be called repeatedly until it returns `UnicodeDecodingResult.EmptyInput`.
    /// Checking that the generator was exhausted is not sufficient.  The decoder
    /// can have an internal buffer that is pre-filled with data from the input
    /// generator.
    ///
    /// Because of buffering, it is impossible to find the corresponing position
    /// in the generator for a given returned `UnicodeScalar` or an error.
    ///
    /// - parameter next: A *generator* of code units to be decoded.
    mutating func decode<G : GeneratorType where G.Element == CodeUnit>(inout next: G) -> UnicodeDecodingResult

    /// Encode a `UnicodeScalar` as a series of `CodeUnit`s by
    /// `put`'ing each `CodeUnit` to `output`.
    static func encode<S : SinkType where S.Element == CodeUnit>(input: UnicodeScalar, inout output: S)
}


/// The result of one Unicode decoding step.
///
/// A unicode scalar value, an indication that no more unicode scalars
/// are available, or an indication of a decoding error.
enum UnicodeDecodingResult {
    case Result(UnicodeScalar)
    case EmptyInput
    case Error

    /// Return true if `self` indicates no more unicode scalars are
    /// available.
    func isEmptyInput() -> Bool
}


/// A [Unicode scalar value](http://www.unicode.org/glossary/#unicode_scalar_value).
struct UnicodeScalar : UnicodeScalarLiteralConvertible {

    /// A numeric representation of `self`.
    var value: UInt32 { get }
    init(_builtinUnicodeScalarLiteral value: Builtin.Int32)

    /// Create an instance initialized to `value`.
    init(unicodeScalarLiteral value: UnicodeScalar)

    /// Creates an instance of the NUL scalar value.
    init()

    /// Create an instance with numeric value `v`.
    ///
    /// - Requires: `v` is a valid Unicode scalar value.
    init(_ v: UInt32)

    /// Create an instance with numeric value `v`.
    ///
    /// - Requires: `v` is a valid Unicode scalar value.
    init(_ v: UInt16)

    /// Create an instance with numeric value `v`.
    init(_ v: UInt8)

    /// Create a duplicate of `v`.
    init(_ v: UnicodeScalar)

    /// Return a String representation of `self` .
    ///
    /// - parameter forceASCII: If `true`, forces most values into a numeric
    ///   representation.
    func escape(asASCII forceASCII: Bool) -> String

    /// Returns true if this is an ASCII character (code point 0 to 127
    /// inclusive).
    func isASCII() -> Bool
}

extension UnicodeScalar : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension UnicodeScalar : Streamable {

    /// Write a textual representation of `self` into `target`.
    func writeTo<Target : OutputStreamType>(inout target: Target)
}

extension UnicodeScalar : CustomStringConvertible, CustomDebugStringConvertible {

    /// A textual representation of `self`.
    var description: String { get }

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }
}

extension UnicodeScalar : Hashable {

    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    var hashValue: Int { get }
}

extension UnicodeScalar {

    /// Construct with value `v`.
    ///
    /// - Requires: `v` is a valid unicode scalar value.
    init(_ v: Int)
}

extension UnicodeScalar : Equatable, Comparable {
}

extension UnicodeScalar {
}

extension UnicodeScalar.UTF16View : CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType {
}


/// Conforming types can be initialized with string literals
/// containing a single [Unicode scalar value](http://www.unicode.org/glossary/#unicode_scalar_value).
protocol UnicodeScalarLiteralConvertible {
    typealias UnicodeScalarLiteralType

    /// Create an instance initialized to `value`.
    init(unicodeScalarLiteral value: Self.UnicodeScalarLiteralType)
}


/// The default type for an otherwise-unconstrained unicode scalar literal.
typealias UnicodeScalarType = String


/// A type for propagating an unmanaged object reference.
///
/// When you use this type, you become partially responsible for
/// keeping the object alive.
struct Unmanaged<T> {

    /// Unsafely turn an opaque C pointer into an unmanaged
    /// class reference.
    ///
    /// This operation does not change reference counts.
    ///
    ///     let str: CFString = Unmanaged.fromOpaque(ptr).takeUnretainedValue()
    static func fromOpaque(value: COpaquePointer) -> Unmanaged<T>

    /// Unsafely turn an unmanaged class reference into an opaque
    /// C pointer.
    ///
    /// This operation does not change reference counts.
    ///
    ///     let str: CFString = Unmanaged.fromOpaque(ptr).takeUnretainedValue()
    func toOpaque() -> COpaquePointer

    /// Create an unmanaged reference with an unbalanced retain.
    /// The object will leak if nothing eventually balances the retain.
    ///
    /// This is useful when passing an object to an API which Swift
    /// does not know the ownership rules for, but you know that the
    /// API expects you to pass the object at +1.
    static func passRetained(value: T) -> Unmanaged<T>

    /// Create an unmanaged reference without performing an unbalanced
    /// retain.
    ///
    /// This is useful when passing a reference to an API which Swift
    /// does not know the ownership rules for, but you know that the
    /// API expects you to pass the object at +0.
    ///
    ///     CFArraySetValueAtIndex(.passUnretained(array), i,
    ///                            .passUnretained(object))
    static func passUnretained(value: T) -> Unmanaged<T>

    /// Get the value of this unmanaged reference as a managed
    /// reference without consuming an unbalanced retain of it.
    ///
    /// This is useful when a function returns an unmanaged reference
    /// and you know that you're not responsible for releasing the result.
    func takeUnretainedValue() -> T

    /// Get the value of this unmanaged reference as a managed
    /// reference and consume an unbalanced retain of it.
    ///
    /// This is useful when a function returns an unmanaged reference
    /// and you know that you're responsible for releasing the result.
    func takeRetainedValue() -> T

    /// Perform an unbalanced retain of the object.
    func retain() -> Unmanaged<T>

    /// Perform an unbalanced release of the object.
    func release()

    /// Perform an unbalanced autorelease of the object.
    func autorelease() -> Unmanaged<T>
}


/// A non-owning pointer to buffer of  `T`s stored
/// contiguously in memory, presenting a `Collection` interface to the
/// underlying elements.
struct UnsafeBufferPointer<T> : CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType {

    /// Always zero, which is the index of the first element in a
    /// non-empty buffer.
    var startIndex: Int { get }

    /// The "past the end" position; always identical to `count`.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    var endIndex: Int { get }
    subscript (i: Int) -> T { get }

    /// Construct an UnsafePointer over the `count` contiguous
    /// `T` instances beginning at `start`.
    init(start: UnsafePointer<T>, count: Int)

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> UnsafeBufferPointerGenerator<T>

    /// A pointer to the first element of the buffer.
    var baseAddress: UnsafePointer<T> { get }

    /// The number of elements in the buffer.
    var count: Int { get }
}

extension UnsafeBufferPointer : CustomDebugStringConvertible {

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }
}


/// A generator for the elements in the buffer referenced by
/// `UnsafeBufferPointer` or `UnsafeMutableBufferPointer`.
struct UnsafeBufferPointerGenerator<T> : GeneratorType, SequenceType {

    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    mutating func next() -> T?

    /// `UnsafeBufferPointerGenerator` is also a `SequenceType`, so it
    /// `generate`'s a copy of itself.
    func generate() -> UnsafeBufferPointerGenerator<T>
}


/// A non-owning pointer to buffer of mutable `T`s stored
/// contiguously in memory, presenting a `Collection` interface to the
/// underlying elements.
struct UnsafeMutableBufferPointer<T> : MutableCollectionType, CollectionType, SequenceType, _CollectionDefaultsType, _CollectionGeneratorDefaultsType {

    /// Always zero, which is the index of the first element in a
    /// non-empty buffer.
    var startIndex: Int { get }

    /// The "past the end" position; always identical to `count`.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    var endIndex: Int { get }
    subscript (i: Int) -> T { get nonmutating set }

    /// Construct an UnsafeMutablePointer over the `count` contiguous
    /// `T` instances beginning at `start`.
    init(start: UnsafeMutablePointer<T>, count: Int)

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> UnsafeBufferPointerGenerator<T>

    /// A pointer to the first element of the buffer.
    var baseAddress: UnsafeMutablePointer<T> { get }

    /// The number of elements in the buffer.
    var count: Int { get }
}

extension UnsafeMutableBufferPointer : CustomDebugStringConvertible {

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }
}


/// A pointer to an object of type `T`.  This type provides no automated
/// memory management, and therefore the user must take care to allocate
/// and free memory appropriately.
///
/// The pointer can be in one of the following states:
///
/// - memory is not allocated (for example, pointer is null, or memory has
///   been deallocated previously);
///
/// - memory is allocated, but value has not been initialized;
///
/// - memory is allocated and value is initialized.
struct UnsafeMutablePointer<T> : RandomAccessIndexType, BidirectionalIndexType, ForwardIndexType, _Incrementable, Equatable, Strideable, Comparable, _Strideable, Hashable, NilLiteralConvertible, _PointerType {

    /// Construct a null pointer.
    init()

    /// Convert from an opaque C pointer to a typed pointer.
    ///
    /// This is a fundamentally unsafe conversion.
    init(_ other: COpaquePointer)

    /// Construct an `UnsafeMutablePointer` from a given address in memory.
    ///
    /// This is a fundamentally unsafe conversion.
    init(bitPattern: Word)

    /// Construct an `UnsafeMutablePointer` from a given address in memory.
    ///
    /// This is a fundamentally unsafe conversion.
    init(bitPattern: UWord)

    /// Convert from an `UnsafeMutablePointer` of a different type.
    ///
    /// This is a fundamentally unsafe conversion.
    init<U>(_ from: UnsafeMutablePointer<U>)

    /// Convert from a `UnsafePointer` of a different type.
    ///
    /// This is a fundamentally unsafe conversion.
    init<U>(_ from: UnsafePointer<U>)

    /// Create an instance initialized with `nil`.
    init(nilLiteral: ())

    /// Allocate memory for `num` objects of type `T`.
    ///
    /// - Postcondition: The memory is allocated, but not initialized.
    static func alloc(num: Int) -> UnsafeMutablePointer<T>

    /// Deallocate `num` objects.
    ///
    /// - parameter num: Number of objects to deallocate.  Should match exactly
    ///   the value that was passed to `alloc()` (partial deallocations are not
    ///   possible).
    ///
    /// - Precondition: The memory is not initialized.
    ///
    /// - Postcondition: The memory has been deallocated.
    func dealloc(num: Int)

    /// Access the underlying raw memory, getting and setting values.
    var memory: T { get nonmutating set }

    /// Initialize the value the pointer points to, to construct
    /// an object where there was no object previously stored.
    ///
    /// - Precondition: The memory is not initialized.
    ///
    /// - Postcondition: The memory is initalized; the value should eventually
    ///   be destroyed or moved from to avoid leaks.
    func initialize(newvalue: T)

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
    func move() -> T

    /// Assign from `count` values beginning at source into initialized
    /// memory, proceeding from the first element to the last.
    func assignFrom(source: UnsafeMutablePointer<T>, count: Int)

    /// Assign from `count` values beginning at `source` into
    /// initialized memory, proceeding from the last value to the first.
    /// Use this for assigning ranges into later memory that may overlap
    /// with the source range.
    ///
    /// - Requires: Either `source` precedes `self` or follows `self + count`.
    func assignBackwardFrom(source: UnsafeMutablePointer<T>, count: Int)

    /// Move `count` values beginning at source into raw memory,
    /// transforming the source values into raw memory.
    func moveInitializeFrom(source: UnsafeMutablePointer<T>, count: Int)

    /// Move `count` values beginning at `source` into uninitialized memory,
    /// transforming the source values into raw memory, proceeding from
    /// the last value to the first.  Use this for copying ranges into
    /// later memory that may overlap with the source range.
    ///
    /// - Requires: Either `source` precedes `self` or follows `self + count`.
    func moveInitializeBackwardFrom(source: UnsafeMutablePointer<T>, count: Int)

    /// Copy `count` values beginning at source into raw memory.
    ///
    /// - Precondition: The memory is not initialized.
    ///
    /// - Requires: `self` and `source` may not overlap.
    func initializeFrom(source: UnsafeMutablePointer<T>, count: Int)

    /// Copy the elements of `C` into raw memory.
    ///
    /// - Precondition: The memory is not initialized.
    func initializeFrom<C : CollectionType where C.Generator.Element == T>(source: C)

    /// Assign from `count` values beginning at `source` into initialized
    /// memory, transforming the source values into raw memory.
    ///
    /// - Requires: The `self` and `source` ranges may not overlap.
    func moveAssignFrom(source: UnsafeMutablePointer<T>, count: Int)

    /// Destroy the object the pointer points to.
    ///
    /// - Precondition: The memory is initialized.
    ///
    /// - Postcondition: The value has been destroyed and the memory must
    ///   be initialized before being used again.
    func destroy()

    /// Destroy the `count` objects the pointer points to.
    /// - Precondition: The memory is initialized.
    ///
    /// - Postcondition: The value has been destroyed and the memory must
    ///   be initialized before being used again.
    func destroy(count: Int)
    subscript (i: Int) -> T { get nonmutating set }

    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    var hashValue: Int { get }

    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    func successor() -> UnsafeMutablePointer<T>

    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    func predecessor() -> UnsafeMutablePointer<T>

    /// Return the minimum number of applications of `successor` or
    /// `predecessor` required to reach `other` from `self`.
    ///
    /// - Complexity: O(1).
    func distanceTo(x: UnsafeMutablePointer<T>) -> Int

    /// Return `self` offset by `n` steps.
    ///
    /// - Returns: If `n > 0`, the result of applying `successor` to
    ///   `self` `n` times.  If `n < 0`, the result of applying
    ///   `predecessor` to `self` `-n` times. Otherwise, `self`.
    ///
    /// - Complexity: O(1).
    func advancedBy(n: Int) -> UnsafeMutablePointer<T>
}

extension UnsafeMutablePointer : CustomDebugStringConvertible {

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }
}

extension UnsafeMutablePointer : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension UnsafeMutablePointer : SinkType {
    mutating func put(x: T)
}

extension UnsafeMutablePointer : CVarArgType {
}


/// A pointer to an object of type `T`.  This type provides no automated
/// memory management, and therefore the user must take care to allocate
/// and free memory appropriately.
///
/// The pointer can be in one of the following states:
///
/// - memory is not allocated (for example, pointer is null, or memory has
///   been deallocated previously);
///
/// - memory is allocated, but value has not been initialized;
///
/// - memory is allocated and value is initialized.
struct UnsafePointer<T> : RandomAccessIndexType, BidirectionalIndexType, ForwardIndexType, _Incrementable, Equatable, Strideable, Comparable, _Strideable, Hashable, NilLiteralConvertible, _PointerType {

    /// Construct a null pointer.
    init()

    /// Convert from an opaque C pointer to a typed pointer.
    ///
    /// This is a fundamentally unsafe conversion.
    init(_ other: COpaquePointer)

    /// Construct an `UnsafePointer` from a given address in memory.
    ///
    /// This is a fundamentally unsafe conversion.
    init(bitPattern: Word)

    /// Construct an `UnsafePointer` from a given address in memory.
    ///
    /// This is a fundamentally unsafe conversion.
    init(bitPattern: UWord)

    /// Convert from an `UnsafeMutablePointer` of a different type.
    ///
    /// This is a fundamentally unsafe conversion.
    init<U>(_ from: UnsafeMutablePointer<U>)

    /// Convert from a `UnsafePointer` of a different type.
    ///
    /// This is a fundamentally unsafe conversion.
    init<U>(_ from: UnsafePointer<U>)

    /// Create an instance initialized with `nil`.
    init(nilLiteral: ())

    /// Access the underlying raw memory, getting values.
    var memory: T { get }
    subscript (i: Int) -> T { get }

    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
    ///
    /// - Note: The hash value is not guaranteed to be stable across
    ///   different invocations of the same program.  Do not persist the
    ///   hash value across program runs.
    var hashValue: Int { get }

    /// Returns the next consecutive value after `self`.
    ///
    /// - Requires: The next value is representable.
    func successor() -> UnsafePointer<T>

    /// Returns the previous consecutive value before `self`.
    ///
    /// - Requires: The previous value is representable.
    func predecessor() -> UnsafePointer<T>

    /// Return the minimum number of applications of `successor` or
    /// `predecessor` required to reach `other` from `self`.
    ///
    /// - Complexity: O(1).
    func distanceTo(x: UnsafePointer<T>) -> Int

    /// Return `self` offset by `n` steps.
    ///
    /// - Returns: If `n > 0`, the result of applying `successor` to
    ///   `self` `n` times.  If `n < 0`, the result of applying
    ///   `predecessor` to `self` `-n` times. Otherwise, `self`.
    ///
    /// - Complexity: O(1).
    func advancedBy(n: Int) -> UnsafePointer<T>
}

extension UnsafePointer : CustomDebugStringConvertible {

    /// A textual representation of `self`, suitable for debugging.
    var debugDescription: String { get }
}

extension UnsafePointer : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension UnsafePointer : CVarArgType {
}


/// A set of common requirements for Swift's unsigned integer types.
protocol UnsignedIntegerType : _UnsignedIntegerType, IntegerType {
}


/// An object that can manage the lifetime of storage backing a
/// `CVaListPointer`.
final class VaListBuilder {
}


/// The empty tuple type.
///
/// This is the default return type of functions for which no explicit
/// return type is specified.
typealias Void = ()


/// A signed integer type that occupies one machine word.
typealias Word = Int


/// A sequence of pairs built out of two underlying sequences, where
/// the elements of the `i`th pair are the `i`th elements of each
/// underlying sequence.
struct Zip2<Sequence1 : SequenceType, Sequence2 : SequenceType> : SequenceType {
    typealias Stream1 = Sequence1.Generator
    typealias Stream2 = Sequence2.Generator

    /// A type whose instances can produce the elements of this
    /// sequence, in order.
    typealias Generator = ZipGenerator2<Sequence1.Generator, Sequence2.Generator>

    /// Construct an instance that makes pairs of elements from `sequence1` and
    /// `sequence2`.
    init(_ sequence1: Sequence1, _ sequence2: Sequence2)

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// - Complexity: O(1).
    func generate() -> ZipGenerator2<Sequence1.Generator, Sequence2.Generator>
}


/// A generator for the `Zip2` sequence.
struct ZipGenerator2<Generator1 : GeneratorType, Generator2 : GeneratorType> : GeneratorType {

    /// The type of element returned by `next()`.
    typealias Element = (Generator1.Element, Generator2.Element)

    /// Construct around a pair of underlying generators.
    init(_ generator1: Generator1, _ generator2: Generator2)

    /// Advance to the next element and return it, or `nil` if no next
    /// element exists.
    ///
    /// - Requires: `next()` has not been applied to a copy of `self`
    ///   since the copy was made, and no preceding call to `self.next()`
    ///   has returned `nil`.
    mutating func next() -> (Generator1.Element, Generator2.Element)?
}

func ^(lhs: Int32, rhs: Int32) -> Int32

func ^(lhs: UInt16, rhs: UInt16) -> UInt16

func ^(lhs: Int64, rhs: Int64) -> Int64

func ^(lhs: UInt64, rhs: UInt64) -> UInt64

func ^(lhs: UInt, rhs: UInt) -> UInt

func ^(lhs: Int, rhs: Int) -> Int

func ^<T : _RawOptionSetType>(a: T, b: T) -> T

func ^(lhs: Int16, rhs: Int16) -> Int16

func ^(lhs: Int8, rhs: Int8) -> Int8

func ^(lhs: UInt8, rhs: UInt8) -> UInt8

func ^(lhs: UInt32, rhs: UInt32) -> UInt32

func ^=(inout lhs: Int, rhs: Int)

func ^=(inout lhs: Int32, rhs: Int32)

func ^=<T : BitwiseOperationsType>(inout lhs: T, rhs: T)

func ^=(inout lhs: UInt32, rhs: UInt32)

func ^=(inout lhs: UInt64, rhs: UInt64)

func ^=(inout lhs: Int16, rhs: Int16)

func ^=(inout lhs: UInt16, rhs: UInt16)

func ^=(inout lhs: Int8, rhs: Int8)

func ^=(inout lhs: UInt8, rhs: UInt8)

func ^=(inout lhs: Int64, rhs: Int64)

func ^=(inout lhs: UInt, rhs: UInt)


/// The underlying buffer for an ArrayType conforms to
/// `_ArrayBufferType`.  This buffer does not provide value semantics.
protocol _ArrayBufferType : MutableCollectionType {

    /// The type of elements stored in the buffer.
    typealias Element

    /// Create an empty buffer.
    init()

    /// Adopt the storage of x.
    init(_ buffer: _ContiguousArrayBuffer<Self.Element>)
    subscript (index: Int) -> Self.Element { get nonmutating set }

    /// If this buffer is backed by a uniquely-referenced mutable
    /// `_ContiguousArrayBuffer` that can be grown in-place to allow the `self`
    /// buffer store `minimumCapacity` elements, returns that buffer.
    /// Otherwise, returns nil.
    ///
    /// - Note: The result's baseAddress may not match ours, if we are a
    ///   _SliceBuffer.
    ///
    /// - Note: This function must remain mutating; otherwise the buffer
    ///   may acquire spurious extra references, which will cause
    ///   unnecessary reallocation.
    mutating func requestUniqueMutableBackingBuffer(minimumCapacity: Int) -> _ContiguousArrayBuffer<Self.Element>?

    /// Returns true iff this buffer is backed by a uniquely-referenced mutable
    /// _ContiguousArrayBuffer.
    ///
    /// - Note: This function must remain mutating; otherwise the buffer
    ///   may acquire spurious extra references, which will cause
    ///   unnecessary reallocation.
    mutating func isMutableAndUniquelyReferenced() -> Bool

    /// If this buffer is backed by a `_ContiguousArrayBuffer`
    /// containing the same number of elements as `self`, return it.
    /// Otherwise, return `nil`.
    func requestNativeBuffer() -> _ContiguousArrayBuffer<Self.Element>?

    /// Replace the given `subRange` with the first `newCount` elements of
    /// the given collection.
    ///
    /// - Requires: This buffer is backed by a uniquely-referenced
    /// `_ContiguousArrayBuffer`.
    mutating func replace<C : CollectionType where C.Generator.Element == Element>(subRange subRange: Range<Int>, with newCount: Int, elementsOf newValues: C)
    subscript (subRange: Range<Int>) -> _SliceBuffer<Self.Element> { get }

    /// Call `body(p)`, where `p` is an `UnsafeBufferPointer` over the
    /// underlying contiguous storage.  If no such storage exists, it is
    /// created on-demand.
    func withUnsafeBufferPointer<R>(@noescape body: (UnsafeBufferPointer<Self.Element>) -> R) -> R

    /// Call `body(p)`, where `p` is an `UnsafeMutableBufferPointer`
    /// over the underlying contiguous storage.
    ///
    /// - Requires: Such contiguous storage exists or the buffer is empty.
    mutating func withUnsafeMutableBufferPointer<R>(@noescape body: (UnsafeMutableBufferPointer<Self.Element>) -> R) -> R

    /// The number of elements the buffer stores.
    var count: Int { get set }

    /// The number of elements the buffer can store without reallocation.
    var capacity: Int { get }

    /// An object that keeps the elements stored in this buffer alive.
    var owner: AnyObject { get }

    /// If the elements are stored contiguously, a pointer to the first
    /// element. Otherwise, `nil`.
    var baseAddress: UnsafeMutablePointer<Self.Element> { get }

    /// A value that identifies the storage used by the buffer.  Two
    /// buffers address the same elements when they have the same
    /// identity and count.
    var identity: UnsafePointer<Void> { get }
}

protocol _ArrayType : __ArrayType, RangeReplaceableCollectionType, MutableSliceable, ArrayLiteralConvertible {

    /// Construct an empty Array.
    init()

    /// Construct an array of `count` elements, each initialized to `repeatedValue`.
    init(count: Int, repeatedValue: Self.Generator.Element)

    /// The number of elements the Array stores.
    var count: Int { get }

    /// The number of elements the Array can store without reallocation.
    var capacity: Int { get }

    /// `true` if and only if the Array is empty.
    var isEmpty: Bool { get }
    subscript (index: Int) -> Self.Generator.Element { get set }

    /// Reserve enough space to store minimumCapacity elements.
    ///
    /// - Postcondition: `capacity >= minimumCapacity` and the array has
    ///   mutable contiguous storage.
    ///
    /// - Complexity: O(`count`).
    mutating func reserveCapacity(minimumCapacity: Int)

    /// Append newElement to the Array in O(1) (amortized).
    mutating func append(newElement: Self.Generator.Element)

    /// Append elements from `sequence` to the Array.
    mutating func extend<S : SequenceType where S.Generator.Element == Generator.Element>(sequence: S)

    /// Operator form of `extend`.
    func +=<S : SequenceType where S.Generator.Element == Generator.Element>(inout lhs: Self, rhs: S)

    /// Remove an element from the end of the Array in O(1).
    ///
    /// - returns: The removed element.
    ///
    /// - Requires: `count > 0`.
    mutating func removeLast() -> Self.Generator.Element

    /// Insert `newElement` at index `i`.
    ///
    /// Invalidates all indices with respect to `self`.
    ///
    /// - Complexity: O(`self.count`).
    ///
    /// - Requires: `atIndex <= count`.
    mutating func insert(newElement: Self.Generator.Element, atIndex i: Int)

    /// Remove and return the element at the given index.
    ///
    /// - returns: The removed element.
    ///
    /// - Complexity: Worst case O(N).
    ///
    /// - Requires: `count > index`.
    mutating func removeAtIndex(index: Int) -> Self.Generator.Element

    /// Erase all the elements.  If `keepCapacity` is `true`, `capacity`
    /// will not change.
    mutating func removeAll(keepCapacity keepCapacity: Bool)
    func join<S : SequenceType where S.Generator.Element == Self>(elements: S) -> Self
    init(_ buffer: Self._Buffer)
}


/// Some types require alignment greater than Word on some architectures.
protocol _CVarArgAlignedType : CVarArgType {
}


/// Floating point types need to be passed differently on x86_64
/// systems.  CoreGraphics uses this to make CGFloat work properly.
protocol _CVarArgPassedAsDouble : CVarArgType {
}


/// Effectively a proxy for NSString that doesn't mention it by
/// name.  NSString's conformance to this protocol is declared in
/// Foundation.
@objc protocol _CocoaStringType {
}

protocol _CollectionDefaultsType : SequenceType {

    /// A type that represents a valid position in the collection.
    ///
    /// Valid indices consist of the position of every element and a
    /// "past the end" position that's not valid for use as a subscript.
    typealias Index : ForwardIndexType

    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    var startIndex: Self.Index { get }

    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    var endIndex: Self.Index { get }

    /// Returns the first element of `self`, or `nil` if `self` is empty.
    var first: Self.Generator.Element? { get }
}

extension _CollectionDefaultsType {

    /// Returns `true` iff `self` is empty.
    var isEmpty: Bool { get }

    /// Returns a value less than or equal to the number of elements in
    /// `self`, *nondestructively*.
    ///
    /// - Complexity: O(N).
    func underestimateCount() -> Int

    /// Returns the number of elements.
    ///
    /// - Complexity: O(1) if `Index` conforms to `RandomAccessIndexType`;
    ///   O(N) otherwise.
    var count: Self.Index.Distance { get }
}




/// This protocol is an implementation detail of `CollectionType`; do
/// not use it directly.
///
/// Its requirements are inherited by `CollectionType` and thus must
/// be satisfied by types conforming to that protocol.
protocol _CollectionGeneratorDefaultsType {
    typealias Index : ForwardIndexType
    subscript (position: Self.Index) -> Self._Element { get }
    var startIndex: Self.Index { get }
    var endIndex: Self.Index { get }
}

extension _CollectionGeneratorDefaultsType {
    func generate() -> IndexingGenerator<Self>
}


/// Conforming types can be initialized with color literals (e.g.
/// `[#Color(colorLiteralRed: 1, blue: 0, green: 0, alpha: 1)#]`).
protocol _ColorLiteralConvertible {
    init(colorLiteralRed: Float, green: Float, blue: Float, alpha: Float)
}


/// A container is destructor safe if whether it may store to memory on
/// destruction only depends on its type parameters.
/// For example, whether `Array<T>` may store to memory on destruction depends
/// only on `T`.
/// If `T` is an `Int` we know the `Array<Int>` does not store to memory during
/// destruction. If `T` is an arbitrary class `Array<MemoryUnsafeDestructorClass>`
/// then the compiler will deduce may store to memory on destruction because
/// `MemoryUnsafeDestructorClass`'s destructor may store to memory on destruction.
protocol _DestructorSafeContainer {
}


/// Optionals of conforming types can be initialized with image literals (e.g.
/// `[#Image(imageLiteral: "hi.png")#]`).
protocol _ImageLiteralConvertible {
    init?(imageLiteral: String)
}


/// This protocol is an implementation detail of `ForwardIndexType`; do
/// not use it directly.
///
/// Its requirements are inherited by `ForwardIndexType` and thus must
/// be satisfied by types conforming to that protocol.
protocol _Incrementable : Equatable {

    /// Return the next consecutive value in a discrete sequence of
    /// `Self` values.
    ///
    /// - Requires: `self` has a well-defined successor.
    func successor() -> Self
}




/// This protocol is an implementation detail of `IntegerArithmeticType`; do
/// not use it directly.
///
/// Its requirements are inherited by `IntegerArithmeticType` and thus must
/// be satisfied by types conforming to that protocol.
protocol _IntegerArithmeticType {

    /// Add `lhs` and `rhs`, returning a result and a `Bool` that is
    /// true iff the operation caused an arithmetic overflow.
    static func addWithOverflow(lhs: Self, _ rhs: Self) -> (Self, overflow: Bool)

    /// Subtract `lhs` and `rhs`, returning a result and a `Bool` that is
    /// true iff the operation caused an arithmetic overflow.
    static func subtractWithOverflow(lhs: Self, _ rhs: Self) -> (Self, overflow: Bool)

    /// Multiply `lhs` and `rhs`, returning a result and a `Bool` that is
    /// true iff the operation caused an arithmetic overflow.
    static func multiplyWithOverflow(lhs: Self, _ rhs: Self) -> (Self, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning a result and a `Bool` that is
    /// true iff the operation caused an arithmetic overflow.
    static func divideWithOverflow(lhs: Self, _ rhs: Self) -> (Self, overflow: Bool)

    /// Divide `lhs` and `rhs`, returning the remainder and a `Bool` that is
    /// true iff the operation caused an arithmetic overflow.
    static func remainderWithOverflow(lhs: Self, _ rhs: Self) -> (Self, overflow: Bool)
}


/// This protocol is an implementation detail of `IntegerType`; do
/// not use it directly.
///
/// Its requirements are inherited by `IntegerType` and thus must
/// be satisfied by types conforming to that protocol.
protocol _IntegerType : IntegerLiteralConvertible, CustomStringConvertible, Hashable, IntegerArithmeticType, BitwiseOperationsType, _Incrementable {
}


/// A shadow for the "core operations" of NSArray.
///
/// Covers a set of operations everyone needs to implement in order to
/// be a useful `NSArray` subclass.
@objc protocol _NSArrayCoreType : _NSCopyingType, _NSFastEnumerationType {
    func objectAtIndex(index: Int) -> AnyObject
    func getObjects(_: UnsafeMutablePointer<AnyObject>, range: _SwiftNSRange)
    func countByEnumeratingWithState(state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>, objects: UnsafeMutablePointer<AnyObject>, count: Int) -> Int
    var count: Int { get }
}


/// A shadow for the `NSCopying` protocol.
@objc protocol _NSCopyingType : _ShadowProtocol {
    func copyWithZone(zone: _SwiftNSZone) -> AnyObject
}


/// A shadow for the "core operations" of NSDictionary.
///
/// Covers a set of operations everyone needs to implement in order to
/// be a useful `NSDictionary` subclass.
@objc protocol _NSDictionaryCoreType : _NSCopyingType, _NSFastEnumerationType {
    init(objects: UnsafePointer<AnyObject?>, forKeys: UnsafePointer<Void>, count: Int)
    var count: Int { get }
    func objectForKey(aKey: AnyObject) -> AnyObject?
    func keyEnumerator() -> _NSEnumeratorType
    func copyWithZone(zone: _SwiftNSZone) -> AnyObject
    func getObjects(objects: UnsafeMutablePointer<AnyObject>, andKeys keys: UnsafeMutablePointer<AnyObject>)
    func countByEnumeratingWithState(state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>, objects: UnsafeMutablePointer<AnyObject>, count: Int) -> Int
}


/// A shadow for the API of `NSDictionary` we will use in the core
/// stdlib.
///
/// `NSDictionary` operations, in addition to those on
/// `_NSDictionaryCoreType`, that we need to use from the core stdlib.
/// Distinct from `_NSDictionaryCoreType` because we don't want to be
/// forced to implement operations that `NSDictionary` already
/// supplies.
@objc protocol _NSDictionaryType : _NSDictionaryCoreType {
    func getObjects(objects: UnsafeMutablePointer<AnyObject>, andKeys keys: UnsafeMutablePointer<AnyObject>)
}


/// A shadow for the `NSEnumerator` class.
@objc protocol _NSEnumeratorType : _ShadowProtocol {
    init()
    func nextObject() -> AnyObject?
}


/// A shadow for the `NSFastEnumeration` protocol.
@objc protocol _NSFastEnumerationType : _ShadowProtocol {
    func countByEnumeratingWithState(state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>, objects: UnsafeMutablePointer<AnyObject>, count: Int) -> Int
}


/// A shadow for the "core operations" of NSSet.
///
/// Covers a set of operations everyone needs to implement in order to
/// be a useful `NSSet` subclass.
@objc protocol _NSSetCoreType : _NSCopyingType, _NSFastEnumerationType {
    init(objects: UnsafePointer<AnyObject?>, count: Int)
    var count: Int { get }
    func member(object: AnyObject) -> AnyObject?
    func objectEnumerator() -> _NSEnumeratorType
    func copyWithZone(zone: _SwiftNSZone) -> AnyObject
    func countByEnumeratingWithState(state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>, objects: UnsafeMutablePointer<AnyObject>, count: Int) -> Int
}


/// A shadow for the API of NSSet we will use in the core
/// stdlib.
///
/// `NSSet` operations, in addition to those on
/// `_NSSetCoreType`, that we need to use from the core stdlib.
/// Distinct from `_NSSetCoreType` because we don't want to be
/// forced to implement operations that `NSSet` already
/// supplies.
@objc protocol _NSSetType : _NSSetCoreType {
}

@objc protocol _NSStringCoreType : _NSCopyingType, _NSFastEnumerationType {
    func length() -> Int
    func characterAtIndex(index: Int) -> UInt16
}


/// A Swift Array or Dictionary of types conforming to
/// `_ObjectiveCBridgeable` can be passed to Objective-C as an NSArray or
/// NSDictionary, respectively.  The elements of the resulting NSArray
/// or NSDictionary will be the result of calling `_bridgeToObjectiveC`
/// on each elmeent of the source container.
protocol _ObjectiveCBridgeable {
}


/// A stdlib-internal protocol modeled by the intrinsic pointer types,
/// UnsafeMutablePointer, UnsafePointer, and
/// AutoreleasingUnsafeMutablePointer.
protocol _PointerType {
}


/// This protocol is an implementation detail of `RawOptionSetType`; do
/// not use it directly.
///
/// Its requirements are inherited by `RawOptionSetType` and thus must
/// be satisfied by types conforming to that protocol.
protocol _RawOptionSetType : RawRepresentable, Equatable {
    typealias RawValue : BitwiseOperationsType, Equatable
    init(rawValue: Self.RawValue)
}

@objc protocol _ShadowProtocol {
}


/// This protocol is an implementation detail of `SignedIntegerType`;
/// do not use it directly.
///
/// Its requirements are inherited by `SignedIntegerType` and thus
/// must be satisfied by types conforming to that protocol.
protocol _SignedIntegerType : _IntegerType, SignedNumberType {

    /// Represent this number using Swift's widest native signed integer
    /// type.
    func toIntMax() -> IntMax

    /// Convert from Swift's widest signed integer type, trapping on
    /// overflow.
    init(_: IntMax)
}


/// This protocol is an implementation detail of `Sliceable`; do
/// not use it directly.
///
/// Its requirements are inherited by `Sliceable` and thus must
/// be satisfied by types conforming to that protocol.
protocol _Sliceable : CollectionType {
}


/// This protocol is an implementation detail of `Strideable`; do
/// not use it directly.
///
/// Its requirements are inherited by `Strideable` and thus must
/// be satisfied by types conforming to that protocol.
protocol _Strideable {

    /// A type that can represent the distance between two values of `Self`.
    typealias Stride : SignedNumberType

    /// Returns a stride `x` such that `self.advancedBy(x)` approximates
    /// `other`.
    ///
    /// - Complexity: O(1).
    ///
    /// - SeeAlso: `RandomAccessIndexType`'s `distanceTo`, which provides a
    ///   stronger semantic guarantee.
    func distanceTo(other: Self) -> Self.Stride

    /// Returns a `Self` `x` such that `self.distanceTo(x)` approximates
    /// `n`.
    ///
    /// - Complexity: O(1).
    ///
    /// - SeeAlso: `RandomAccessIndexType`'s `advancedBy`, which
    ///   provides a stronger semantic guarantee.
    func advancedBy(n: Self.Stride) -> Self
}


/// Instances of conforming types are used in internal `String`
/// representation.
protocol _StringElementType {
}


/// This protocol is an implementation detail of `SignedIntegerType`;
/// do not use it directly.
///
/// Its requirements are inherited by `SignedIntegerType` and thus
/// must be satisfied by types conforming to that protocol.
protocol _UnsignedIntegerType : _IntegerType {

    /// Represent this number using Swift's widest native unsigned
    /// integer type.
    func toUIntMax() -> UIntMax

    /// Convert from Swift's widest unsigned integer type, trapping on
    /// overflow.
    init(_: UIntMax)
}

protocol __ArrayType : CollectionType {
    var count: Int { get }
}


/// Return the absolute value of `x`.
///
/// Concrete instances of `SignedNumberType` can specialize this
/// function by conforming to `AbsoluteValuable`.
func abs<T : SignedNumberType>(x: T) -> T


/// Return the result of advancing start by `n` positions, or until it
/// equals `end`.  If `T` models `RandomAccessIndexType`, executes in
/// O(1).  Otherwise, executes in O(`abs(n)`).  If `T` does not model
/// `BidirectionalIndexType`, requires that `n` is non-negative.
func advance<T : ForwardIndexType>(start: T, _ n: T.Distance, _ end: T) -> T


/// Return the result of advancing `start` by `n` positions.  If `T`
/// models `RandomAccessIndexType`, executes in O(1).  Otherwise,
/// executes in O(`abs(n)`).  If `T` does not model
/// `BidirectionalIndexType`, requires that `n` is non-negative.
func advance<T : ForwardIndexType>(start: T, _ n: T.Distance) -> T


/// Returns the minimum memory alignment of `T`.
func alignof<T>(_: T.Type) -> Int


/// Returns the minimum memory alignment of `T`.
func alignofValue<T>(_: T) -> Int


/// Return a `GeneratorType` instance that wraps `base` but whose type
/// depends only on the type of `G.Element`.
///
/// Example:
///
///     func countStrings() -> AnyGenerator<String> {
///       let lazyStrings = lazy(0..<10).map { String($0) }
///
///       // This is a really complicated type of no interest to our
///       // clients.
///       let g: MapSequenceGenerator<RangeGenerator<Int>, String>
///         = lazyStrings.generate()
///       return anyGenerator(g)
///     }
func anyGenerator<G : GeneratorType>(base: G) -> AnyGenerator<G.Element>


/// Return a `GeneratorType` instance whose `next` method invokes
/// `body` and returns the result.
///
/// Example:
///
///     var x = 7
///     let g = anyGenerator { x < 15 ? x++ : nil }
///     let a = Array(g) // [ 7, 8, 9, 10, 11, 12, 13, 14 ]
func anyGenerator<T>(body: () -> T?) -> AnyGenerator<T>


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
func assert(@autoclosure condition: () -> Bool, @autoclosure _ message: () -> String = default, file: StaticString = default, line: UWord = default)


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
func assertionFailure(@autoclosure message: () -> String = default, file: StaticString = default, line: UWord = default)

func debugPrint<T, TargetStream : OutputStreamType>(value: T, inout _ target: TargetStream)


/// Writes the textual representation of `value` most suitable for debugging,
/// and an optional newline, into the stream `target`.
///
/// The textual representation is obtained from the `value` using its protocol
/// conformances, in the following order of preference:
/// `CustomDebugStringConvertible`, `CustomStringConvertible`, `Streamable`.
/// If none of these conformances are found, a default text representation is
/// constructed in an implementation-defined way, based on the type kind and
/// structure.
///
/// Do not overload this function for your type.  Instead, adopt one of the
/// protocols mentioned above.
///
/// - parameter appendNewline: Iff `true` (the default), write a trailing
///   newline.
func debugPrint<T, TargetStream : OutputStreamType>(value: T, inout _ target: TargetStream, appendNewline: Bool)

func debugPrint<T>(value: T)


/// Writes the textual representation of `value` most suitable for debugging,
/// and an optional newline, into the standard output.
///
/// The textual representation is obtained from the `value` using its protocol
/// conformances, in the following order of preference:
/// `CustomDebugStringConvertible`, `CustomStringConvertible`, `Streamable`.
/// If none of these conformances are found, a default text representation is
/// constructed in an implementation-defined way, based on the type kind and
/// structure.
///
/// Do not overload this function for your type.  Instead, adopt one of the
/// protocols mentioned above.
///
/// - parameter appendNewline: Iff `true` (the default), write a trailing
///   newline.
func debugPrint<T>(value: T, appendNewline: Bool)


/// Measure the distance between `start` and `end`.
///
/// If `T` models `RandomAccessIndexType`, requires that `start` and `end` are
/// part of the same sequence, and executes in O(1).
///
/// Otherwise, requires that `end` is reachable from `start` by
/// incrementation, and executes in O(N), where N is the function's
/// result.
func distance<T : ForwardIndexType>(start: T, _ end: T) -> T.Distance


/// Returns a slice containing all but the first element of `s`.
///
/// - Requires: `s` is non-empty.
func dropFirst<Seq : Sliceable>(s: Seq) -> Seq.SubSlice


/// Returns a slice containing all but the last element of `s`.
///
/// - Requires: `s` is non-empty.
func dropLast<S : Sliceable where S.Index : BidirectionalIndexType>(s: S) -> S.SubSlice


/// Dump an object's contents using its mirror to the specified output stream.
func dump<T, TargetStream : OutputStreamType>(x: T, inout _ targetStream: TargetStream, name: String? = default, indent: Int = default, maxDepth: Int = default, maxItems: Int = default) -> T


/// Dump an object's contents using its mirror to standard output.
func dump<T>(x: T, name: String? = default, indent: Int = default, maxDepth: Int = default, maxItems: Int = default) -> T


/// Append elements from `newElements` to `x`.
///
/// - Complexity: O(N).
func extend<C : RangeReplaceableCollectionType, S : CollectionType where S.Generator.Element == C.Generator.Element>(inout x: C, _ newElements: S)


/// Unconditionally print a `message` and stop execution.
@noreturn func fatalError(@autoclosure message: () -> String = default, file: StaticString = default, line: UWord = default)


/// Returns a `CVaListPointer` built from `args` that's backed by
/// autoreleased storage.
///
/// - Warning: This function is best avoided in favor of
///   `withVaList`, but occasionally (i.e. in a `class` initializer) you
///   may find that the language rules don't allow you to use
/// `withVaList` as intended.
func getVaList(args: [CVarArgType]) -> CVaListPointer


/// Insert `newElement` into `x` at index `i`.
///
/// Invalidates all indices with respect to `x`.
///
/// - Complexity: O(`x.count`).
func insert<C : RangeReplaceableCollectionType>(inout x: C, _ newElement: C.Generator.Element, atIndex i: C.Index)


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
func isUniquelyReferenced<T : NonObjectiveCBase>(inout object: T) -> Bool


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
func isUniquelyReferencedNonObjC<T>(inout object: T?) -> Bool


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
func isUniquelyReferencedNonObjC<T>(inout object: T) -> Bool


/// Creates and returns a collection of type `C` that is the result of
/// interposing a given separator between the elements of the sequence
/// `elements`.
///
/// For example, this code excerpt writes "``here be dragons``" to the standard
/// output:
///
///     print(join(" ", [ "here", "be", "dragons" ]))
func join<C : ExtensibleCollectionType, S : SequenceType where S.Generator.Element == C>(separator: C, _ elements: S) -> C


/// Augment `s` with lazy methods such as `map`, `filter`, etc.
func lazy<S : SequenceType>(s: S) -> LazySequence<S>


/// Augment `s` with lazy methods such as `map`, `filter`, etc.
func lazy<S : CollectionType where S.Index : RandomAccessIndexType>(s: S) -> LazyRandomAccessCollection<S>


/// Augment `s` with lazy methods such as `map`, `filter`, etc.
func lazy<S : CollectionType where S.Index : BidirectionalIndexType>(s: S) -> LazyBidirectionalCollection<S>


/// Augment `s` with lazy methods such as `map`, `filter`, etc.
func lazy<S : CollectionType where S.Index : ForwardIndexType>(s: S) -> LazyForwardCollection<S>


/// Returns the greatest argument passed.
func max<T : Comparable>(x: T, _ y: T, _ z: T, _ rest: T...) -> T


/// Returns the greater of `x` and `y`.
func max<T : Comparable>(x: T, _ y: T) -> T


/// Returns the least argument passed.
func min<T : Comparable>(x: T, _ y: T, _ z: T, _ rest: T...) -> T


/// Returns the lesser of `x` and `y`.
func min<T : Comparable>(x: T, _ y: T) -> T


/// Convert `x` to type `U`, trapping on overflow in -Onone and -O
/// builds.
///
/// Typically used to do conversion to any contextually-deduced
/// integer type:
///
///     func f(x: Int32) {}
///     func g(x: Int64) { f(numericCast(x)) }
func numericCast<T : _SignedIntegerType, U : _SignedIntegerType>(x: T) -> U


/// Convert `x` to type `U`, trapping on overflow in -Onone and -O
/// builds.
///
/// Typically used to do conversion to any contextually-deduced
/// integer type:
///
///     func f(x: UInt32) {}
///     func g(x: UInt64) { f(numericCast(x)) }
func numericCast<T : _UnsignedIntegerType, U : _UnsignedIntegerType>(x: T) -> U


/// Convert `x` to type `U`, trapping on overflow in -Onone and -O
/// builds.
///
/// Typically used to do conversion to any contextually-deduced
/// integer type:
///
///     func f(x: UInt32) {}
///     func g(x: Int64) { f(numericCast(x)) }
func numericCast<T : _SignedIntegerType, U : _UnsignedIntegerType>(x: T) -> U


/// Convert `x` to type `U`, trapping on overflow in -Onone and -O
/// builds.
///
/// Typically used to do conversion to any contextually-deduced
/// integer type:
///
///     func f(x: Int32) {}
///     func g(x: UInt64) { f(numericCast(x)) }
func numericCast<T : _UnsignedIntegerType, U : _SignedIntegerType>(x: T) -> U


/// Returns `true` if `lhs` and `rhs` have a non-empty intersection.
func overlaps<I0 : IntervalType, I1 : IntervalType where I0.Bound == I1.Bound>(lhs: I0, _ rhs: I1) -> Bool


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
func precondition(@autoclosure condition: () -> Bool, @autoclosure _ message: () -> String = default, file: StaticString = default, line: UWord = default)


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
@noreturn func preconditionFailure(@autoclosure message: () -> String = default, file: StaticString = default, line: UWord = default)


/// Returns a slice, up to `maxLength` in length, containing the
/// initial elements of `s`.
///
/// If `maxLength` exceeds `s.count`, the result contains all
/// the elements of `s`.
///
/// - Complexity: O(1)+K when `S.Index` conforms to
///   `RandomAccessIndexType` and O(N)+K otherwise, where K is the cost
///   of slicing `s`.
func prefix<S : Sliceable>(s: S, _ maxLength: Int) -> S.SubSlice


/// Writes the textual representation of `value`, and an optional newline,
/// into the stream `target`.
///
/// The textual representation is obtained from the `value` using its protocol
/// conformances, in the following order of preference: `Streamable`,
/// `CustomStringConvertible`, `CustomDebugStringConvertible`.  If none of
/// these conformances are found, a default text representation is constructed
/// in an implementation-defined way, based on the type kind and structure.
///
/// Do not overload this function for your type.  Instead, adopt one of the
/// protocols mentioned above.
///
/// - parameter appendNewline: Iff `true` (the default), write a trailing
///   newline.
func print<T, TargetStream : OutputStreamType>(value: T, inout _ target: TargetStream, appendNewline: Bool)

func print<T, TargetStream : OutputStreamType>(value: T, inout _ target: TargetStream)


/// Writes the textual representation of `value`, and an optional newline,
/// into the standard output.
///
/// The textual representation is obtained from the `value` using its protocol
/// conformances, in the following order of preference: `Streamable`,
/// `CustomStringConvertible`, `CustomDebugStringConvertible`.  If none of
/// these conformances are found, a default text representation is constructed
/// in an implementation-defined way, based on the type kind and structure.
///
/// Do not overload this function for your type.  Instead, adopt one of the
/// protocols mentioned above.
///
/// - parameter appendNewline: Iff `true` (the default), write a trailing
///   newline.
func print<T>(value: T, appendNewline: Bool)

func print<T>(value: T)


/// Returns `Character`s read from standard input through the end of the
/// current line or until EOF is reached, or `nil` if EOF has already been
/// reached.
///
/// If `stripNewline` is `true`, newline characters and character
/// combinations will be stripped from the result.  This is the default.
///
/// Standard input is interpreted as `UTF-8`.  Invalid bytes
/// will be replaced by Unicode [replacement characters](http://en.wikipedia.org/wiki/Specials_(Unicode_block)#Replacement_character).
func readLine(stripNewline stripNewline: Bool = default) -> String?


/// Produce a mirror for any value. If the value's type conforms to Reflectable,
/// invoke its getMirror() method; otherwise, fall back to an implementation
/// in the runtime that structurally reflects values of any type.
func reflect<T>(x: T) -> MirrorType


/// Remove all elements from `x`.
///
/// Invalidates all indices with respect to `x`.
///
/// - parameter keepCapacity: If `true`, is a non-binding request to
///    avoid releasing storage, which can be a useful optimization
///    when `x` is going to be grown again.
///
/// - Complexity: O(`x.count`).
func removeAll<C : RangeReplaceableCollectionType>(inout x: C, keepCapacity: Bool = default)


/// Remove from `x` and return the element at index `i`.
///
/// Invalidates all indices with respect to `x`.
///
/// - Complexity: O(`x.count`).
func removeAtIndex<C : RangeReplaceableCollectionType>(inout x: C, _ index: C.Index) -> C.Generator.Element


/// Remove an element from the end of `x`  in O(1).
///
/// - Requires: `x` is nonempty.
func removeLast<C : RangeReplaceableCollectionType where C.Index : BidirectionalIndexType>(inout x: C) -> C.Generator.Element


/// Remove from `x` the indicated `subRange` of elements.
///
/// Invalidates all indices with respect to `x`.
///
/// - Complexity: O(`x.count`).
func removeRange<C : RangeReplaceableCollectionType>(inout x: C, _ subRange: Range<C.Index>)


/// Returns the contiguous memory footprint of `T`.
///
/// Does not include any dynamically-allocated or "remote" storage.
/// In particular, `sizeof(X.self)`, when `X` is a class type, is the
/// same regardless of how many stored properties `X` has.
func sizeof<T>(_: T.Type) -> Int


/// Returns the contiguous memory footprint of  `T`.
///
/// Does not include any dynamically-allocated or "remote" storage.
/// In particular, `sizeof(a)`, when `a` is a class instance, is the
/// same regardless of how many stored properties `a` has.
func sizeofValue<T>(_: T) -> Int

func sort<T : Comparable>(inout array: ContiguousArray<T>)


/// Insert `newElements` into `x` at index `i`.
///
/// Invalidates all indices with respect to `x`.
///
/// - Complexity: O(`x.count + newElements.count`).
func splice<C : RangeReplaceableCollectionType, S : CollectionType where S.Generator.Element == C.Generator.Element>(inout x: C, _ newElements: S, atIndex i: C.Index)


/// Returns the result of slicing `elements` into sub-sequences that
/// don't contain elements satisfying the predicate `isSeparator`.
///
/// - parameter maxSplit: The maximum number of slices to return, minus 1.
///   If `maxSplit + 1` slices would otherwise be returned, the
///   algorithm stops splitting and returns a suffix of `elements`.
///
/// - parameter allowEmptySlices: If `true`, an empty slice is produced in
///   the result for each pair of consecutive.
func split<S : Sliceable, R : BooleanType>(elements: S, maxSplit: Int = default, allowEmptySlices: Bool = default, @noescape isSeparator: (S.Generator.Element) -> R) -> [S.SubSlice]


/// Return the sequence of values (`start`, `start + stride`, `start +
/// stride + stride`, ... *last*) where *last* is the last value in
/// the progression less than or equal to `end`.
///
/// - Note: There is no guarantee that `end` is an element of the sequence.
func stride<T : Strideable>(from start: T, through end: T, by stride: T.Stride) -> StrideThrough<T>


/// Return the sequence of values (`start`, `start + stride`, `start +
/// stride + stride`, ... *last*) where *last* is the last value in
/// the progression that is less than `end`.
func stride<T : Strideable>(from start: T, to end: T, by stride: T.Stride) -> StrideTo<T>


/// Returns the least possible interval between distinct instances of
/// `T` in memory.  The result is always positive.
func strideof<T>(_: T.Type) -> Int


/// Returns the least possible interval between distinct instances of
/// `T` in memory.  The result is always positive.
func strideofValue<T>(_: T) -> Int


/// Returns a slice, up to `maxLength` in length, containing the
/// final elements of `s`.
///
/// If `maxLength` exceeds `s.count`, the result contains all
/// the elements of `s`.
///
/// - Complexity: O(1)+K when `S.Index` conforms to
///   `RandomAccessIndexType` and O(N)+K otherwise, where K is the cost
///   of slicing `s`.
func suffix<S : Sliceable where S.Index : BidirectionalIndexType>(s: S, _ maxLength: Int) -> S.SubSlice


/// Exchange the values of `a` and `b`.
func swap<T>(inout a: T, inout _ b: T)


/// Translate `input`, in the given `InputEncoding`, into `output`, in
/// the given `OutputEncoding`.
///
/// - parameter stopOnError: Causes encoding to stop when an encoding
///   error is detected in `input`, if `true`.  Otherwise, U+FFFD
///   replacement characters are inserted for each detected error.
func transcode<Input : GeneratorType, Output : SinkType, InputEncoding : UnicodeCodecType, OutputEncoding : UnicodeCodecType where InputEncoding.CodeUnit == Input.Element, OutputEncoding.CodeUnit == Output.Element>(inputEncoding: InputEncoding.Type, _ outputEncoding: OutputEncoding.Type, _ input: Input, inout _ output: Output, stopOnError: Bool) -> Bool


/// Returns an `UnsafePointer` to the storage used for `object`.  There's
/// not much you can do with this other than use it to identify the
/// object.
func unsafeAddressOf(object: AnyObject) -> UnsafePointer<Void>


/// Returns the the bits of `x`, interpreted as having type `U`.
///
/// - Warning: Breaks the guarantees of Swift's type system; use
///   with extreme care.  There's almost always a better way to do
///   anything.
///
func unsafeBitCast<T, U>(x: T, _: U.Type) -> U


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
func unsafeDowncast<T>(x: AnyObject) -> T


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
func unsafeUnwrap<T>(nonEmpty: T?) -> T


/// Evaluate `f()` and return its result, ensuring that `x` is not
/// destroyed before f returns.
func withExtendedLifetime<T, Result>(x: T, @noescape _ f: () -> Result) -> Result


/// Evaluate `f(x)` and return its result, ensuring that `x` is not
/// destroyed before f returns.
func withExtendedLifetime<T, Result>(x: T, @noescape _ f: T -> Result) -> Result


/// Invokes `body` with an `UnsafeMutablePointer` to `arg` and returns the
/// result. Useful for calling Objective-C APIs that take "in/out"
/// parameters (and default-constructible "out" parameters) by pointer.
func withUnsafeMutablePointer<T, Result>(inout arg: T, @noescape _ body: UnsafeMutablePointer<T> -> Result) -> Result


/// Like `withUnsafeMutablePointer`, but passes pointers to `arg0`, `arg1`,
/// and `arg2`.
func withUnsafeMutablePointers<A0, A1, A2, Result>(inout arg0: A0, inout _ arg1: A1, inout _ arg2: A2, @noescape _ body: (UnsafeMutablePointer<A0>, UnsafeMutablePointer<A1>, UnsafeMutablePointer<A2>) -> Result) -> Result


/// Like `withUnsafeMutablePointer`, but passes pointers to `arg0` and `arg1`.
func withUnsafeMutablePointers<A0, A1, Result>(inout arg0: A0, inout _ arg1: A1, @noescape _ body: (UnsafeMutablePointer<A0>, UnsafeMutablePointer<A1>) -> Result) -> Result


/// Invokes `body` with an `UnsafePointer` to `arg` and returns the
/// result. Useful for calling Objective-C APIs that take "in/out"
/// parameters (and default-constructible "out" parameters) by pointer.
func withUnsafePointer<T, Result>(inout arg: T, @noescape _ body: UnsafePointer<T> -> Result) -> Result


/// Like `withUnsafePointer`, but passes pointers to `arg0` and `arg1`.
func withUnsafePointers<A0, A1, Result>(inout arg0: A0, inout _ arg1: A1, @noescape _ body: (UnsafePointer<A0>, UnsafePointer<A1>) -> Result) -> Result


/// Like `withUnsafePointer`, but passes pointers to `arg0`, `arg1`,
/// and `arg2`.
func withUnsafePointers<A0, A1, A2, Result>(inout arg0: A0, inout _ arg1: A1, inout _ arg2: A2, @noescape _ body: (UnsafePointer<A0>, UnsafePointer<A1>, UnsafePointer<A2>) -> Result) -> Result


/// Invoke `f` with a C `va_list` argument derived from `args`.
func withVaList<R>(args: [CVarArgType], @noescape _ f: CVaListPointer -> R) -> R


/// Invoke `f` with a C `va_list` argument derived from `builder`.
func withVaList<R>(builder: VaListBuilder, @noescape _ f: CVaListPointer -> R) -> R


/// A sequence of pairs built out of two underlying sequences, where
/// the elements of the `i`th pair are the `i`th elements of each
/// underlying sequence.
func zip<Sequence1 : SequenceType, Sequence2 : SequenceType>(sequence1: Sequence1, _ sequence2: Sequence2) -> Zip2<Sequence1, Sequence2>

func |(lhs: Int32, rhs: Int32) -> Int32

func |(lhs: UInt64, rhs: UInt64) -> UInt64

func |(lhs: UInt32, rhs: UInt32) -> UInt32

func |(lhs: Int64, rhs: Int64) -> Int64

func |(lhs: Int16, rhs: Int16) -> Int16

func |(lhs: UInt16, rhs: UInt16) -> UInt16

func |(lhs: UInt, rhs: UInt) -> UInt

func |(lhs: Int8, rhs: Int8) -> Int8

func |(lhs: Int, rhs: Int) -> Int

func |(lhs: UInt8, rhs: UInt8) -> UInt8

func |<T : _RawOptionSetType>(a: T, b: T) -> T

func |=(inout lhs: Int64, rhs: Int64)

func |=(inout lhs: UInt32, rhs: UInt32)

func |=(inout lhs: UInt, rhs: UInt)

func |=(inout lhs: Int16, rhs: Int16)

func |=(inout lhs: UInt16, rhs: UInt16)

func |=(inout lhs: UInt64, rhs: UInt64)

func |=(inout lhs: Int32, rhs: Int32)

func |=(inout lhs: Int, rhs: Int)

func |=(inout lhs: Int8, rhs: Int8)

func |=(inout lhs: UInt8, rhs: UInt8)

func |=<T : BitwiseOperationsType>(inout lhs: T, rhs: T)


/// If `lhs` is `true`, return it.  Otherwise, evaluate `rhs` and
/// return its `boolValue`.
func ||<T : BooleanType, U : BooleanType>(lhs: T, @autoclosure rhs: () -> U) -> Bool

func ||<T : BooleanType>(lhs: T, @autoclosure rhs: () -> Bool) -> Bool

prefix func ~(rhs: Int64) -> Int64

prefix func ~(rhs: Int32) -> Int32

prefix func ~(rhs: UInt64) -> UInt64

prefix func ~(rhs: UInt) -> UInt

prefix func ~<T : _RawOptionSetType>(a: T) -> T

prefix func ~(rhs: Int8) -> Int8

prefix func ~(rhs: UInt32) -> UInt32

prefix func ~(rhs: Int) -> Int

prefix func ~(rhs: UInt8) -> UInt8

prefix func ~(rhs: Int16) -> Int16

prefix func ~(rhs: UInt16) -> UInt16

func ~=<T>(lhs: _OptionalNilComparisonType, rhs: T?) -> Bool

func ~=<T : Equatable>(a: T, b: T) -> Bool


/// Returns `true` iff `pattern` contains `value`.
func ~=<I : IntervalType>(pattern: I, value: I.Bound) -> Bool

func ~=<I : ForwardIndexType where I : Comparable>(pattern: Range<I>, value: I) -> Bool

