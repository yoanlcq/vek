[![Travis CI Build Status](https://travis-ci.org/yoanlcq/vek.svg?branch=master)](https://travis-ci.org/yoanlcq/vek)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/ir0d4pkpkfwv643q/branch/master?svg=true)](https://ci.appveyor.com/project/yoanlcq/vek/branch/master)
[![Documentation](https://docs.rs/vek/badge.svg)](https://docs.rs/vek)
[![Version](https://img.shields.io/crates/v/vek.svg)](https://crates.io/crates/vek)
![MIT/Apache-2.0](https://img.shields.io/badge/License-MIT%2FApache--2.0-blue.svg)
[![Downloads](https://img.shields.io/crates/d/vek.svg)](https://crates.io/crates/vek)

# vek

Generic but efficient SIMD vector+matrix library for game engines, with focus on intent and the small bits that make you happier as a user.

DO NOT USE (yet). This is very much a work-in progress, breaking changes happen all the time on a whim.


The efficiency claim is based on the fact that implementations are specialized according
to the target hardware, and the generated assembly is checked to ensure it is optimal.  
As you would expect, SSE-enabled x86 CPUs do benefit from this.



# Useful links

- [The top-level documentation](https://docs.rs/vek);
- [The FAQ](#faq);
- [Roadmap to 1.0](https://github.com/yoanlcq/vek/issues/1);
- [The list of "features" that are purposefully not implemented](#wont-implement).



# FAQ

## Why this crate ?

Low-dimensioned matrices and vectors (i.e <= 4 dimensions) are so commonly 
used in multimedia
applications that they deserve to be promoted to core types, and are expected
to be as fast as the hardware allows, at all times.

It is safe to assume
that they are used a LOT, especially when they're processed *en masse*.  
Positions, directions, curves, shapes, colors, texture coordinates, physics
systems, animation systems, and many others... All of them need and 
process such types all the time. When they are fast, a lot of dependent
systems greatly benefit from it.

It is quite a commitment to choose a vector/matrix library for a project - any 
game, engine or library that uses it is implictly tied to its decisions regarding
performance and functionality. When none of these libraries are suitable for the
task, one ends up being written in-house, with a variable quality criteria.

Today, especially with Rust's power, it is possible to unconditionnaly have
genericity, ergonomics, functionality and performance all at once.  
With `vek`, I hope to provide these core types as generics, while 
generating high-quality assembly output on specific hardware, when
appropriate types are used (and you're on Nightly with the `repr_simd` and
`repr_align` features enabled).

Let's have our cake and eat it too !


## Can I use it on Stable ?

Yes! You only have to disable the `repr_simd` and `repr_aligned` features.


## This crate is so slow to compile!

Disable default features, then selectively enable the ones you need.  
It's unlikely that you need absolutely all of `vek`'s types, so pick the few
that you'll actually use.  

On my machine, restricting features to only include the `Mat4` and `Vec4` types
made build times go from 24 seconds to 3 seconds!

Also, disabling the `repr_simd` feature should approximately divide build times by two.


## How do I shuffle a vector ?

With the ergonomic, idiomatic way to shuffle any struct in Rust!  
You want **destructuring**:

```rust
let Xyzw { x, y, .. } = xyzw;
let wzyx = Xyzw { x: w, y: z, z: y, w: x };
let xyxx = Xyzw { x, y, z: x, w: x };
```

But don't take my word for it - let the (release mode) assembly speak for itself!  
On x86 with SSE, it lowers to `shufps` as wanted.

If you're only interested in a single element you can use `broadcast` (or even `from`):

```rust
let Xyzw { x, .. } = xyzw;
let xxxx = Xyzw::broadcast(x);
let xxxx = Xyzw::from(x);
```


# Why do element-wise comparison functions return `SomeVec<u8>` and not `SomeVec<bool>` ?

Because `bool` is not a "machine type", SIMD `bool` vector types can't be monomorphized,
and it's a pain to try to handle this case.
If you really want a vector of `bool`s for some reason, you can use `convert()`:

```rust
let v = Vec4(0,1,0,1);
let b = v.convert(|x| x != 0);
```

This will only work for vectors which are not `#[repr(simd)]`.


## Why can't I index a matrix directly (i.e write `m[1][3]`) ?

Because the actual meaning changes depending on the matrix's storage layout.  
What you describe will probably do what you expect on row-major matrices,
but how should it behave on column-major matrices ? `vek`'s mantra is to stay
true to reality, so if it had to do this, it would just index the
public member `cols` in this case.

But here's the thing - if you decided to switch the layout, no `m[i][j]` access
would behave as expected anymore, and there's no way the compiler could catch this.

For these reasons, in `vek`, it is almost always required that you explicitly
use the matrix's public member instead, which conveys the intent and enforces correctness.  
Here's how you index matrices in `vek` (assuming, for instance, that `i=1` and `j=3`):

- Row-major, static indexing: `(m.rows.1).3`;
- Row-major, dynamic indexing: `m.rows[i][j]`;
- Column-major, static indexing: `(m.cols.3).1`;
- Column-major, dynamic indexing: `m.cols[j][i]`;
- Any layout, dynamic indexing: `m[(i, j)]` (or `m[Vec2(i, j)]`).

In the same way, if you want to get a pointer to the matrix's data, for e.g transferring it
to a graphics API, get the address of the public member explictly instead, which makes clear
whether you're sending an array of rows or an array of columns.  
If you're using OpenGL, check out the `as_gl_uniform_params()` method which is implemented on
`f32` matrices.


## What the heck is a `Xy<T>`? Is spelling out `Vector2` too much for you?

There are two issues: first the rationale behind `Xy<T>`, and second, naming conventions.

For the former:
- `Xy<T>` is a struct that has `x` and `y` members. It has uses as spatial coordinates.
- `Vec2<T>` is a pair of values that have the same type. It's more general-purpose than `Xy<T>`.

For the latter, renaming imports to the rescue!

```rust
use vek::vec::Xy as Vector2;
use vek::mat::Mat4 as Matrix4x4;

let v = Vector2 { x: 13, y: 42 };
// ....
```

Also, I believe the names match Rust's general terseness level.  
Dynamic growable arrays are named `Vec`, not `Vector`, because they're
so widely used that typing more than three characters becomes annoying.  

It's the same reasoning behind names of shell commands such as `ls`.

Also, people accustomed to GLSL are familiar with `mat4` and `vec4`.


## What's the deal with these `repr_simd` and `repr_c` modules ?

At first, when writing generics with `#[repr(simd)]`, it might appear possible 
to write this :

```rust
#[repr(packed,simd)]
pub struct Vec4<T>(pub T, pub T, pub T, pub T);
pub struct Mat4<T> { pub rows: Vec4<Vec4<T>> }
```

And it would indeed compile. However, at monomorphization time, you'll have
compiler errors because `#[repr(simd)]` can only work on vectors of 
"machine types" (and a `#[repr(simd)]` vector is not itself such a 
"machine type" - curiously, neither are `isize` not `usize`).

So this means we _have_ to end up with either this:

```rust
#[repr(packed,simd)]
pub struct Vec4<T>(pub T, pub T, pub T, pub T);
pub struct Mat4<T> { pub rows: [Vec4<T> ; 4] }
```

or this :

```rust
#[repr(packed,simd)]
pub struct Vec4<T>(pub T, pub T, pub T, pub T);
#[repr(packed,C,align(16))]
pub struct CVec4<T>(pub T, pub T, pub T, pub T);
pub struct Mat4<T> { pub rows: CVec4<Vec4<T>> }
```

The latter is what `vek` essentially does, and ensures that `Vec4` and `CVec4`
provide the exact same API. There are more benefits than downsides from
this approach, and more control for us (e.g alignment of structs).
Yes, this does duplicate functionality, and no, it's not likely to be a problem
except for compile times.


## This crate misses one or more optimized operations for my target platform (or, some parts could be improved) !

I care about this. Whether you actually need them or not, file an issue on the 
Github repo and let's see what we can do.  
However please avoid filing a PR before discussing the matter in an issue.


## Why generics?

**TL:DR;**  
First, the historical problem with generics in C++ is the increase in build times,
and possibly insanely long and confusing error message, but none of these apply to Rust.  
Second, we _can_ be generic and still generate efficient code.


As much as 32-bit floating-point happens to be the most common case, the
algorithms are universal, and there's no reason
we wouldn't suddenly switch to, say, fixed-point numbers or bignums.

Fixed-point numbers do provide some goodies: 
- Consistency of results across platforms and compiler options, useful for lockstep networking models;
- They are the only option for representing real numbers on some esoteric platforms such as the Nintendo DS.
So I want to be able to switch to using them painlessly.  

Bignums also come
up as an interesting target, even though we might wonder in which cases we
need more than 64-bit worth of integer values.

On the other hand, one thing that indeed plagues generics is that the code
is written once but over-generalized, such that the compiler can't always
"see" what the actual optimal code for the target hardware is.

`#[repr(simd)]` is good, but not a magic wand. It **will** lower basic vector
operations into shuffles, packed arithmetic operations, etc, but it won't be
able to "guess" when what you just wrote is actually
the "unpcklps" instruction (and there, the generated assembly is an awful
bunch of "shufps" instead).  

It happens that sometimes we **do** want
to use the intrinsics directly, but we still want to be generic!  
So both are provided - that is, you get reasonably efficient code by default,
but still have access to specialized functions under certain assumptions
(for instance, `Mat4<f32>` provides `transposed_sse()` on SSE-enabled x86 CPUs).


## Why aren't "efficient" implementations the default ?

Because most hardware-specific intrinsics have semantics that bypass some of Rust's assumptions,
some of which are :
- Alignment requirements (most load/store instructions);
- Precision of floating-point operations (e.g `_mm_rsqrt_ps()`);
- Handling of integer overflow (e.g `_mm_add_epi32()`);
- Expectations from the user (e.g `_mm_cmpeq_ps()` use `0xffffffff` in the output vector as the value for `true`);

The point is, hardware-specific intrinsics are, well, hardware-specific, which
is why it's up to you to opt-in explicitly.  
The generic implementations may not be as efficient, but won't backstab you either.


## Why don't I get pretty assembly on debug builds ?

You don't actually care in this situations. It's release builds you're after.

Also keep in mind that Rust checks for integer overflows on debug builds, so
e.g your pretty `Vec4<i32>` addition won't be lowered to `paddd` on
SSE2-enabled CPUs.


## Why not [insert related crate here] ?

As some have discussed, the "quality" of a vector/matrix library is often
a matter of personal preference.  
I think it is normal and healthy that there are
so many choices available. It's as healthy as having so many different game engines,
operating systems, Web browsers, countries, and cultures.

So instead of trying to convince you why, for some reason, you shouldn't use one of the
well-written libraries out there such as `cgmath`, `nalgebra` and `vecmath`, I'll try
my best at explaining the problems I want to solve with `vek`.

### 1. I don't want to worry anymore about my vectors and matrices being less efficient than they ought to be.

It's common to assume that the compiler can optimize everything (and often,
it does) but it's a huge oversight for libraries that provide core types.

As a user, you might not realize that the "matrix * vector" products you use
everywhere take twice as many instructions as they should.  
Yes, you won't see the difference in "most cases", but in my (limited) 
experience, "most cases" is "moderately ambitious games running on 
x86-64 CPUs", which is why there's no noticeable slowdown, but that shouldn't
get in the way of "potentially ambitious games running on PC, consoles and 
mobile devices".

SSE and SSE2 have been around since 1999 and 2001 respectively. All x86-64 CPUs
have them, and nearly every PC today is powered by such CPUs.  
According to the [Steam Hardware Survey](http://store.steampowered.com/hwsurvey/),
100% of PCs have SSE2.

So obviously, on such targets, if my `Vec4<f32>` addition doesn't lower to the `addps`
instruction, I'll get very upset.

### 2. I want to be able to choose freely between row-major and column-major.

Row-major matrices have their uses just as well
as column-major ones. Let's end this debate, please.  
One should be allowed to pick the correct one for the
job at any time and place.  

It _happens_ that column-major matrices are good at multiplying themselves by
a column vector, which is the most common case in computer graphics
(because it's how we transform vertices), but this doesn't mean this is
somehow the One True Layout.  

Row-major matrices are good at being the right-hand side of a product with
a row vector. Also, one might just prefer them because of the indexing order.

This all boils down to giving more control to the user. Who am I to decide
on your behalf ?

### 3. Writing `Extent2`, `Rgba` and `Rgb` types from scratch every time gets old fast.

Back when I was using SFML, I would write stuff such as `window.size.x` but
_obviously_ it feels slightly odd.  
The Vulkan API was wise enough to
define `vkExtent2d` and `vkExtent3d` types for representing _spatial extents_, so I
want these types too. What are they ? Plain old vectors. But their members are
named such that it is clear that we're dealing with widths, heights and depths.

The others that come back all too often are `Rgb` and `Rgba`. I need these _all the time_, either
as `Rgba<u8>` of `Rgba<f32>`.

### 4. I want a library that is close to the reality.

"Reality of the hardware" rather than "pretty pink pony mathematical reality".  

I don't want more abstraction. I want compression of information.

I don't want a pretty-looking mathematical model. I want to have access to the
basic building blocks that don't actively try to hide what's actually happening.  
There's no such type as `OrthogonalMat4`, `AffineMat4` or the like. It's a damn `Mat4`, 
because it's what the hardware deals with.  

### 5. I want to be able to have vectors/matrices of fixed-points and bignums.

i.e I don't want to be stuck on floating-point numbers.

### 6. I don't want to have to import a trait for every piece of functionality.

And I don't want a `prelude` either.

Fundamentally, if I'm given an `Rgba` type, I don't want to have to import
some `ColorVector` trait (or some prelude) to be able to call `red()` on it.
The same goes for dot products, identity matrices, and whatever.

However, _of course_ it's practical to make them implement relevant traits.

### 7. I want fully-fledged types with exhaustive abilities.

Not **only** free functions. That's how I used to do it in C (Even back then I used GNU M4 to generate poor-man's generics).  
Now, hopefully, there's Rust, and I certainly prefer writing `m * v` instead of `col_mat4_transform(m, v)`!  

I mean, if there's no easy way in my codebase to talk about a `Mat4` ASAP, I'm in for a serious
productivity (and sanity) loss. I don't want to pollute
my mental cache by asking myself if I have to use fixed-size arrays, tuples,
structs, or tuple structs for my vectors or matrices.  
I want them readily available for use and never have to go back to, or question, their implementation.


# Won't implement

*What follows is a list of features that are purposefully not implemented.*

***

Any iterator over matrices. Iterate over the public member instead.  
Transpose the matrix if needed.

***

`FromIterator` for matrices. Collect the public member instead.

***

Indexing matrices directly, as in `m[i][j]`.  
Rationale :

Most people are tempted to write `m[i][j]` when `i` and `j` are
known at compile-time, and might not realize that in Rust this
semantically implies bounds checking at runtime.  

Preventing them from being able to do this has these advantages:  
First, they'll consider writing e.g `(m.rows.1).3` or `(m.cols.3).1`
instead, which is not only checked at compile-time, but also
explicit and ensures correctness.
If the user suddenly decides to switch layouts, they'll get helpful
compiler errors because the matrix's public member has changed, and
therefore the meaning of indexing is flipped and requires attention.  
We wouldn't have this "correctness check" if writing `m[i][j]` was allowed.

If one still wants dynamic indexing, they can write `m.rows[i][j]` or
`m.cols[j][i]`.  
They can also write `m[(i, j)]` or `m[Vec2(i, j)]`, which has the same semantics
regardless of the storage layout.

***

Matrix division by matrix, because it is too confusing (and not always possible).  
Explicitly mutiply by the inverse (if possible) instead.

***

`swap_*` functions for matrices. People can use `mem::swap()` on members
directly, bypassing bounds checking.

***

`as_ptr()` or `as_mut_ptr()` for matrices, because it's important to be explicit about
giving an array of rows or columns. Explicitly turn the public member
into a pointer instead.  
`as_gl_uniform_params()` is the only exception.

***

`invert_orthogonal()` or `invert_rotation()`.
It's better to explicitly call `transpose()` to invert a matrix that is known to be orthogonal.

***

`Rad` or `Deg` newtypes. They have no way to enforce correctness, and get annoying to use quickly.

Per `vek`'s mantra (i.e stay true to reality), all angles are assumed to be in
radians, because this is the unit that's **actually** used for calculations.  
Degrees are only good for displaying in GUIs, and as such, are none of my concern.  

Unity does this, but it's a **bad decision**, because :
1. It hides the reality;
2. It confuses those who know what they're doing;
3. The actual problem they want to solve is convenience in the inspector, which
   would be better solved in the first place by providing dedicated "angle editor" widgets.

Also floating-point types already have `to_radians()` and `to_degrees()` in Rust.
If you want to support degrees so bad, then I dunno, write your own wrappers or 
properly handle conversions yourself.

***

Indexing on Quaternions. Convert them to a `Xyzw` first instead.

***

Non-square matrices. They're a bit like [EBCDIC](https://en.wikipedia.org/wiki/EBCDIC), in that,
they exist, but darn, who actually uses them ? (don't you raise your hand).

In GLSL ES, there are only square matrix types (only the `NV_non_square_matrices` GLSL extension brings them back).  
Even in desktop GL, non-square matrices are seldom used ([See this StackOverflow question](https://stackoverflow.com/q/36994741)).

The functionality can **still** be emulated by using higher-order square matrix types and setting
appropriate members to zero.  

If you're concerned about the space it takes in memory, don't forget that you can simply
store appropriate vectors of vectors (or even more compact structures) and convert them on-the-fly to square matrices, as needed.

Also non-square matrices are too much of a pain to deal with when generating code via macros for what it's actually worth.
