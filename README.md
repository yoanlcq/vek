[![Travis CI Build Status](https://travis-ci.org/yoanlcq/vek.svg?branch=master)](https://travis-ci.org/yoanlcq/vek)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/ir0d4pkpkfwv643q/branch/master?svg=true)](https://ci.appveyor.com/project/yoanlcq/vek/branch/master)
[![Documentation](https://docs.rs/vek/badge.svg)](https://docs.rs/vek)
[![Version](https://img.shields.io/crates/v/vek.svg)](https://crates.io/crates/vek)
![MIT/Apache-2.0](https://img.shields.io/github/license/yoanlcq/vek.svg)
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

Yes ! Disable the `repr_simd` and `repr_aligned` features, but don't expect
miracles to happen with regards to perf.


## This crate misses one or more optimized operations for my target platform (or, some parts could be improved) !

I care about this. Whether you actually need them or not, file an issue on the 
Github repo and let's see what we can do.  
However please avoid filing a PR before discussing the matter in an issue.


## This crate is so slow to compile!

Try to disable default features, then selectively enable the ones you need.


## Why can't I index a matrix directly (e.g write `m[1][3]`) ?

Because the actual meaning changes depending on the matrix's storage layout.  
What you describe will probably do what you expect on row-major matrices,
but how should it behave on column-major matrices ? `vek`'s mantra is to stay
true to reality, so if it had to do this, it would just index the
public member `cols` in this case.

But here's the thing - if you decided to switch the layout, no `m[i][j]` access
would behave as expected anymore, and there's no way the compiler could catch this.

For these reasons, in `vek`, it is almost always required that you explicitly
use the matrix's public member instead, which conveys the intent and enforces correctness.  
Here's how you index matrices this way (assuming, for instance, that `i=1` and `j=3`):

- Row-major, static indexing: `(m.rows.1).3`;
- Row-major, dynamic indexing: `m.rows[i][j]`;
- Column-major, static indexing: `(m.cols.3).1`;
- Column-major, dynamic indexing: `m.cols[j][i]`;

In the same way, if you want to get a pointer to the matrix's data, for e.g transferring it
to a graphics API, get the address of the public member explictly instead, which makes clear
whether you're sending an array of rows or an array of columns.  
If you're using OpenGL, check out the `as_gl_uniform_params()` method which is implemented on
`f32` matrices.

## What's the deal with these `repr_simd` and `repr_c` modules ?

At first, when writing generics with `#[repr(simd)]`, it might appear possible 
to write this :

```ignore
#[repr(packed,simd)]
pub struct Vec4<T>(pub T, pub T, pub T, pub T);
pub struct Mat4<T> { pub rows: Vec4<Vec4<T>> }
```

And it would indeed compile. However, at monomorphization time, you'll have
compiler errors because `#[repr(simd)]` can only work on vectors of 
"machine types" (and a `#[repr(simd)]` vector is not itself such a 
"machine type" - curiously, neither are `isize` not `usize`).

So this means we _have_ to end up with either this:

```ignore
#[repr(packed,simd)]
pub struct Vec4<T>(pub T, pub T, pub T, pub T);
pub struct Mat4<T> { pub rows: [Vec4<T> ; 4] }
```

or this :

```ignore
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


## Why generics?

As much as 32-bit floating-point happens to be the most common case, the
algorithms are universal, and there's no reason
we wouldn't suddenly switch to, say, fixed-point numbers or bignums.

Fixed-point numbers provide a lot of goodies (e.g consistency of results across 
platforms and compiler options, useful for lockstep networking models) 
and I want to be able to switch to using them painlessly.  
Bignums also come
up as an interesting target, even though we might wonder in which cases we
need more than 64-bit worth of integer values.

On the other hand, one thing that indeed plagues generics is that the code
is written once but over-generalized, such that the compiler can't always
"see" what the actual optimal code for the target hardware is.

`#[repr(simd)]` is good, but not a magic wand. It **will** lower basic vector
operations into shuffles, packed arithmetic operations, etc.  
However, it won't be able to guess when what you've just written is actually
the "unpcklps" instruction (and there, the generated assembly is an awful
bunch of "shufps" instead).  
It happens that sometimes we **do** want
to use the intrinsics directly, but we still want to be generic!

In most other languages, this would be unthinkable, but in
Rust, with modules, macros, and plenty of other ergonomics, this is actually
achievable, in a way not far from professionnal.


## Why provide free functions in the public API ?

Having public target-dependant free frunctions allows users to use them
with run-time feature detection (rather than compile time).

In addition, it does simplify this crate's architecture a bit.


## Why don't I get pretty assembly on debug builds ?

Debug builds are what they are.

Also keep in mind that Rust checks for integer overflows on debug builds, so
e.g your pretty `Vec4<i32>` addition won't be lowered to `paddd` on
SSE2-enabled CPUs.


## Why not [insert related crates here] ?

It probably looks a bit ridiculous to provide so much rationale in this section,
but seeing how many crates I could have used out there instead of writing my own, 
I do feel this need to explain.

**In my (hopefully humble) opinion...**

***
Little to no attention is paid to the quality of the actual assembly output.

It's common to assume that the compiler can optimize everything (and often,
it does) but it's a huge oversight for libraries that provide core types.

As a user, you might not realize that the "matrix * vector" products you use
everywhere with library X take twice as many instructions as they should.  
Yes, You won't see the difference in "most cases", but do define "most cases"!  
In my experience, "most cases" is "moderately ambitious games running on 
x86-64 CPUs", which is why there's no noticeable slowdown, but that shouldn't
get in the way of "potentially ambitious games running on PC, consoles and 
mobile devices".
***
Most libraries only ever provide one storage layout for matrices.

They're not to blame, since GLM does it too (even though it's a global setting
that can be toggled).  
However I think it's evil (and lazy) to just assume that there's only one
layout users could be interested in.

Row-major matrices have their uses just as well
as column-major ones. One should be allowed to pick the correct one for the
job at any time and place.  
For instance, column-major matrices are good at multiplying themselves by
a column vector, which is the most common case in computer graphics
(because it's how we transform vertices), but this doesn't mean this is
somehow the One True Layout.  
Row-major matrices are good at being the right-hand side of a product with
a row vector. Also, one might just prefer them because of the indexing order.
This all boils down to giving more control to the user.
***
Related to the above two, libraries don't clearly state which targets they
were optimized for (and which operations are optimized).

I hope I'm making it clear that even though `vek` focuses on x86, there are
efforts to make it able to reason about, say, ARM and others.
***
Such libraries provide the ubiquitous `Vector` types (which members are
`x`, `y`, `z` and `w`), but no `Rgb`, `Rgba`, `Extent` or `Uv` types, which
really are the same, but represent different intents.

A lack of such types is likely to make people use `Vector2`s for unsuited data
such as a
viewport/window size where an `Extent2` (with `w` and `h` members) would
better convey the intent (and even, in this case, guarantee correctness 
by being unsigned).
***
Some libraries, on the other hand, go crazy with new types (it's not bad, but 
it's essentially more and more abstraction).

I'm thinking about `nalgebra`, which remains a high-quality library, albeit 
a bit too high in abstraction space for 3D game engines.

When in doubt, `vek` favors explicitness and the _reality_ (as in, "reality 
of the hardware" rather than "pretty pink pony mathematical reality").  
This implies there's no `AffineMat3` type or the likes - there are only
matrices. If you want such a type, you're free to build it yourself by
wrapping some functionality, but at the end of the day, you'll probably
convert it to a matrix anyway.
***
Some libraries assume that their element types implement some kind 
of `BaseFloat` trait, which is good design, but now 
they're stuck to only floating-point numbers. 

Welp, no fixed-point or bignums for you!
***
Some libraries provide their functionalities mostly through traits.  
Again, this is good design. By providing a `prelude` module, they allow users
to concisely import functionality without spelling out every trait every time.

It's a matter of personal taste, but I dislike having to do this.  
Fundamentally, if I'm given an `Rgba` type, I don't want to have to import
some `ColorVector` trait (or some prelude) to be able to call `red()` on it.
The same goes for dot products, identity matrices, and whatever.
***
One (`vecmath`) _only_ provides free functions (i.e no methods).

This is how it's done in C, but we can do much better in Rust.  
I do prefer writing `m * v` instead of `col_mat4_transform(m, v)`!  
However, it completely clicks with `vecmath`'s stated goals. Also anyone
could use these free functions as foundations to their own library.

It happens that I've needed (and written) vector and matrix types so much
that I feel like I need some kind of definite, feature-complete, 
"end-all" solution.


# Won't implement

*What follows is a list of features that are purposefully not implemented.*

***

Any iterator over matrices. Iterate over the public member instead.  
Transpose the matrix if needed.

***

`FromIterator` for matrices. Collect the public member instead.

***

Indexing matrices directly.  
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

***

Divide matrix by matrix, because it is too confusing (and not always possible).
Explicitly mutiply by the inverse (if possible) instead.

***

`swap_*` functions for matrices. People can use `mem::swap()` on members
directly, bypassing bounds checking.

***

`as_ptr()` or `as_mut_ptr()`, because it's important to be explicit about
giving an array of rows or columns. Explicitly turn the public member
into a pointer instead.  
`as_gl_uniform_params()` is the only exception.

***

`invert_orthogonal()` or `invert_rotation()`.
It's better to explicitly call `transpose()` to invert a matrix that is known to be orthogonal.

***

`Rad` or `Deg` newtypes. It's unhandy and worthless - what do you want to check
for ? 
Also floating-point types already have `to_radians()` and `to_degrees()` in Rust.
Radians are used because they are the only sane angular unit. If you want to support 
degrees so bad, then write your own wrappers.

***

Indexing on Quaternions. Convert them to a `Xyzw` first instead.

***

Non-square matrices. It's a bit like EBCDIC, it exists but nobody actually uses it
(in the context `vek` would be used, at least).
The functionality can be emulated by using higher-order square matrix types and setting
appropriate members to zero.
If you're concerned about the space it takes in memory, don't forget that you can simply
store vectors of vectors and convert them on-the-fly to square matrices, as needed.
