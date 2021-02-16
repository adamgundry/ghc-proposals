HasField redesign
=================

.. author:: Adam Gundry
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. contents::


Motivation
----------

Following `proposal #6 <https://github.com/ghc-proposals/ghc-proposals/pull/6>`_,
GHC 8.2 introduced a special built-in typeclass ``HasField`` in the
``GHC.Records`` module, defined thus::

  class HasField (x :: k) r a | x r -> a where
    getField :: r -> a

When the constraint solver sees a constraint of the form ``HasField "foo" T a``,
where ``T`` is a concrete datatype and ``foo`` is a symbol corresponding to one
of its fields, and this field is in scope, the constraint will be solved
automatically with a dictionary derived from the record selector function for
the field.

This makes it possible to get a form of type-directed name resolution for field
names: given the expression ``getField @"foo" t``, the inferred type of ``t``
can be used to determine which ``foo`` field is meant, even if there are
multiple ``foo`` fields in scope and hence the expression ``foo t`` would be
ambiguous.  (This arises in particular with the ``DuplicateRecordFields``
extension, which has a somewhat ad hoc mechanism for disambiguating such
expressions that is to be removed following `proposal #366
<https://github.com/ghc-proposals/ghc-proposals/pull/366>`_.)

However, the status quo is lacking in two important respects:

1. There is no facility for updating fields.

2. The syntax ``getField @"foo"`` is rather convoluted.

As a result, ``HasField`` has seen relatively little use to date.  Several more
recent proposals have suggested changes to address this; they are recapitulated
in subsequent sections.  In particular, `proposal #158
<https://github.com/ghc-proposals/ghc-proposals/pull/158>`_ plans to change the
definition of ``HasField`` to support updates, and `proposal #282
<https://github.com/ghc-proposals/ghc-proposals/pull/282>`_ introduces a new
``RecordDotSyntax`` extension to provide better syntax.  In the light of
experience implementing these proposals, it seems worth systematically
re-evaluating the design choices surrounding ``HasField``.


Recap: Planned changes to HasField
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The accepted `proposal #158
<https://github.com/ghc-proposals/ghc-proposals/pull/158>`_ plans to change the
definitions in ``GHC.Records`` to the following::

  class HasField (x :: k) r a | x r -> a where
    hasField :: r -> (a -> r, a)

  getField :: forall x r a . HasField x r a => r -> a
  getField = snd . hasField @x

  setField :: forall x r a . HasField x r a => r -> a -> r
  setField = fst . hasField @x

This makes it possible to both get and set fields, based on a single class.  An
`implementation of proposal #158
<https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3257>`_ is available on a
GHC branch, but has not yet been merged, because the compile-time performance
cost of the selected implementation strategy is unacceptably high.  Such costs
were not really considered in previous discussions, but it is not appropriate to
slow down compilation of all programs with records for the benefit only of those
using ``HasField``.


Recap: Planned RecordDotSyntax extension
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The accepted `proposal #282
<https://github.com/ghc-proposals/ghc-proposals/pull/282>`_ defined a new
extension ``RecordDotSynax``, which adds syntax for record selection using
dot-notation, and changes the interpretation of the existing record update
syntax.  For example, under the proposed extension, ``t.foo`` translates to
``getField @"foo"`` and ``t { foo = e }`` translates to ``setField @"foo" t e``.
The latter relies on the planned introduction of ``setField`` from
`proposal #158 <https://github.com/ghc-proposals/ghc-proposals/pull/158>`_.


Recap: OverloadedLabels
~~~~~~~~~~~~~~~~~~~~~~~
The ``OverloadedLabels`` extension (see the accepted `proposal #6
<https://github.com/ghc-proposals/ghc-proposals/pull/6>`_) allows an overloaded
label ``#foo`` to be interpreted as a call to
``fromLabel :: IsLabel "foo" a => a``.  This was designed to provide a syntax
for record field selection by giving an ``IsLabel`` instance for the function
space.  However, because of controversy over whether an overloaded label should
be interpreted as a selector function or a van Laarhoven lens, this proposal has
not been implemented fully: ``base`` does not currently define an ``IsLabel``
instance for functions.

It is possible to define one of two orphan ``IsLabel`` instances for functions,
allowing overloaded labels to be used as either record selectors or van
Laarhoven lenses, depending on which instance is defined.  However these cannot
be used simultaneously, so libraries cannot safely depend on them.

The ``optics`` library defines a representation of lenses and other optics that
uses an abstract newtype, rather than a type synonym for a van Laarhoven lens
(as in the ``lens`` library).  Thus it can interpret overloaded labels as optics
without problems.


Design aims and objectives
~~~~~~~~~~~~~~~~~~~~~~~~~~
The aim of this proposal is to redesign ``HasField`` in a way that balances the
interests of different groups of users:

* Those who plan to use ``RecordDotSyntax`` when it is available.

* Those who use optics libraries (such as ``lens`` and ``optics``), and wish to
  have the ability to construct lenses for record fields conveniently.

* Those who do not wish to use ``HasField`` at all, for whom the main
  requirement is that it should not affect them (in particular, it should not
  impose a compile-time performance penalty).

In order to achieve this, this proposal has the following objectives:

* ``HasField`` should offer a foundation for both ``RecordDotSyntax`` and
  optics-based approaches to record operations, without privileging either
  approach.

* As far as possible, type inference behaviour and type error messages directly
  related to ``HasField`` should be easy for users to understand.

* We should not impose a compile-time performance cost on code that does not use
  ``HasField``.  Costs for using ``HasField`` should be comparable to normal
  uses of record syntax.

* The design should be clearly specified, with the various trade-offs
  articulated and considered together.

Equally important are the things that we do not propose to tackle with this
proposal:

* The API provided by the ``GHC.Records`` module itself is not designed to be
  called directly by normal users; nor are they routinely expected to define
  their own ``HasField`` instances.  Rather, the ``GHC.Records`` API should
  provide the necessary internal functionality for user-facing features such as
  ``RecordDotSyntax``, and libraries such as ``optics``.

* Support for anonymous records is not in scope for this proposal. There are
  many design choices around different ways to integrate anonymous records with
  Haskell, and the right way forward is not obvious. ``HasField`` should reflect
  the capabilities of existing Haskell records, and need not offer capabilities
  that are not useful in this context.  (Thus the approach advocated here
  contrasts with e.g. the dormant `proposal #180
  <https://github.com/ghc-proposals/ghc-proposals/pull/180>`_ which seeks to add
  support for row polymorphism.)


Design questions
----------------

Having established overall criteria for the design in the previous section, we
will now review the various specific design choices that arise with
``HasField``, and propose a resolution in each case.


Single class vs. multiple classes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The dormant `proposal #286
<https://github.com/ghc-proposals/ghc-proposals/pull/286>`_ suggests splitting
``HasField`` into two classes, ``GetField`` and ``SetField``.  It is primarily
motivated by the possibility of supporting read-only (virtual) fields.  (There
is no proposed mechanism for normal record fields to be marked as being
read-only or write-only to limit when the constraints should be solved
automatically, but in principle this would be possible.)

Another benefit of splitting into two classes is that it allows more precise
types: a function of type
``(GetField "foo" r Int, SetField "bar" r Bool) => r -> r``
obviously can only read the ``foo`` field and write the ``bar`` field.


Compilation time benefits of splitting classes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
In the implementation of `proposal #158
<https://github.com/ghc-proposals/ghc-proposals/pull/158>`_, it became apparent
that such a split is also desirable for reasons of compile-time performance.
In particular, the existing implementation of ``HasField`` (with only
``getField``) is able to make use of the selector functions that GHC already
generates for all fields.  However this is not possible if ``HasField`` also
must provide the ability to set the field.  In this case, GHC must generate more
complex definitions for ``HasField`` dictionaries.  The initial implementation
generated these at record definition sites, which would impose a nontrivial
compile-time cost on modules with large records, even for programs not making
use of ``HasField`` .  An alternative implementation strategy would be to defer
generating the dictionaries to use sites, which imposes no extra cost when
``HasField`` is not used, but entails unnecessary work when it is used.

By splitting ``HasField`` into two classes, one for selection and one for
update, GHC can continue to make use of the selector functions already generated
at record definition sites, while update functions can be generated as needed at
use sites.  Since record updates are likely to be less frequent than selections,
and traditional record updates already are compiled by generating a suitable
case-statement, this seems like a reasonable performance trade-off.  Moreover,
we can expose `Flags for compile-time performance control`_.


Relationship between ``GetField``, ``SetField`` and ``HasField``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
There are various options for the superclass relationships between the split
classes.  `Proposal #286
<https://github.com/ghc-proposals/ghc-proposals/pull/286>`_ suggests having
``GetField`` be a superclass of ``SetField``, however this would rule out the
possibility of set-only fields and make the dictionary more complex to build at
compile time.

Instead we propose the following design, ignoring `type-changing update`_ and
`functional dependencies`_ for now::

  class GetField x r a where
    getField :: r -> a

  class SetField x r a where
    setField :: r -> a -> r

  type HasField x r a = (GetField x r a, SetField x r a)

Here ``GetField`` and ``SetField`` are independent.  In both cases the
underlying dictionaries are newtypes, and should be comparatively cheap to
construct.

``HasField`` is a constraint synonym for the pair of constraints ``GetField``
and ``SetField``.  Its inclusion means that when both ``getField`` and
``setField`` are used, users can write simpler types, and GHC can (at least in
principle) infer such types.

This change is not entirely backwards compatible.  Existing code using
``HasField`` should mostly continue to work, provided it does not define virtual
fields or use an explicit import ``import GHC.Records (HasField(getField))``.
Code defining virtual fields via explicit ``HasField`` instances will need to be
modified to define instances of ``GetField`` and ``SetField`` instead.


Downsides of keeping the classes independent
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
A potential disadvantage of splitting ``HasField`` into two independent classes
is that where a user defines a "virtual field" that requires indexing into a
data structure (e.g. a map), it may be possible to implement an operation that
gets and modifies a field more efficiently than defining it from ``getField``
and ``setField``.  This is why `proposal #158
<https://github.com/ghc-proposals/ghc-proposals/pull/158>`_ settled on
``hasField :: r -> (a -> r, a)``.

However in practice this is likely to be rare, and does not arise for normal
record types with the built-in constraint-solving behaviour. Where this matters,
users are likely to be better off using an optics library, and can always define
an auxiliary class such as the following::

  class HasField x r a => HasFieldLens x r a where
    fieldLens :: Lens' x r a
    fieldLens = lens getField setField

  instance {-# OVERLAPPABLE #-} HasField x r a => HasFieldLens x r a

We do not propose to add such a class to ``GHC.Records``, since it is better
defined by specific optics libraries.  (The ``optics`` library defines a class
``LabelOptic`` that plays essentially this role.)


Order of arguments to setField
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TODO: propose ``field -> record -> record`` order as it composes better.


Functional dependencies
~~~~~~~~~~~~~~~~~~~~~~~
The existing ``HasField`` class expresses the relationship between the record
type and the field type using a functional dependency::

  class HasField x r a | x r -> a

That is, the field label and record type should together determine the field
type.  This is necessary to allow good type inference.  In particular, it allows
the type of a composition of field selectors to be inferred::

  getField @"foo" . getField @"bar"
    :: (GetField "foo" b c, GetField "bar" a b) => a -> c

The middle type ``b`` appears only in the context, so it would be ambiguous in
the absence of the functional dependency.

Instead of using a functional dependency, it is also possible to express this
using a type family (associated or otherwise), like so::

  class HasField x r where
    type FieldType x r :: Type

    getField :: r -> FieldType x r

With this definition, we obtain::

  getField @"foo" . getField @"bar"
    :: (HasField "foo" (FieldType "bar" a), HasField "bar" a) =>
       a -> FieldType "foo" (FieldType "bar" a)

Introducing such a type family would give more options to optics library
implementers and other power users, and `proposal #286
<https://github.com/ghc-proposals/ghc-proposals/pull/286>`_ suggests making this
change.

However, we propose to retain the use of functional dependencies in the class
definitions, for the following reasons:

* The functional dependency approach generally leads to simpler inferred types
  and clearer type errors, because unsolved constraints look like ``HasField x
  r a`` which has a natural reading "``r`` has a field ``x`` of type ``a``".
  In contrast, the type family approach ends up separately reporting errors
  involving ``HasField x r`` (meaning ``r`` has a field ``x`` of unspecified
  type) and equalities including ``FieldType``.

* In general, the field type may not necessarily be a function of the record
  type.  For example (using ``RankNTypes``)::

    data T = MkT { foo :: forall a . a -> a }

  Here it would in principle make sense to say that ``T`` has a field ``foo`` of
  type ``a -> a`` for any type ``a``, and hence ``HasField "foo" T (a -> a)``
  should be solvable.  (In contrast, the alternative formulation ``HasField
  "foo" T (forall a . a -> a)`` requires an impredicative constraint that cannot
  feasibly be supported, even with the recent introduction of Quick Look
  Impredicativity (`proposal #274
  <https://github.com/ghc-proposals/ghc-proposals/pull/274>`_).

  While this is technically not intentionally supported by functional
  dependencies as they stand, `proposal #374
  <https://github.com/ghc-proposals/ghc-proposals/pull/374>`_ suggests
  optionally allowing such "dysfunctional" dependencies; alternatively it may be
  possible to generalise functional dependencies to a more liberal notion of
  "inference hints" that would permit this (see `#391
  <https://github.com/ghc-proposals/ghc-proposals/issues/391>`_ and the `GHC
  wiki pages on functional dependencies
  <https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC>`_).
  In contrast, there seems to be little hope that such a definition could be
  supported using a ``FieldType`` type family.

* Supporting `Unlifted fields`_ with the type family approach would introduce
  extra complexity (we would need another type family to determine the
  ``RuntimeRep`` of the field).  It is relatively straightforward with
  functional dependencies.

* For `type-changing update`_, it is desirable that either the original or
  updated types may be used to infer the other.  This is difficult to achieve
  with type families.

Functional dependencies do not carry evidence.  This means that from the given
constraints ``(HasField x r a, HasField x r b)`` it would not be possible to
conclude that ``a ~ b``.  However this does not seem like a significant
practical limitation in the ``HasField`` context.


``FieldType`` type family
^^^^^^^^^^^^^^^^^^^^^^^^^
In addition, we propose that ``GHC.Records`` should provide a magic built-in
type family that will determine the type of a field in a record::

  type family FieldType (x :: Symbol) (r :: Type) :: Type

If ``R ...`` is a record type with a field ``foo`` of type ``T`` in scope, GHC
will automatically reduce an occurrence of ``FieldType "foo" (R ...)`` to ``T``.
The type family will not reduce if the field is not in scope, or its type is
higher-rank, existentially quantified or unlifted.

As with ``HasField`` at present, it will be permitted for users to define their
own instances of ``FieldType`` to support "virtual record fields", provided they
do not overlap with the built-in behaviour.

Observe that this type family is independent of the ``(Get|Set|Has)Field`` type
classes, and will not appear in types unless used explicitly in user code.  It
makes it possible to write constraints such as ``HasField x r (FieldType x r)``
and hence satisfy those who would like to have the type family available,
while still using functional dependencies as the primary implementation approach.

It is possible to implement ``FieldType`` using ``GHC.Generics``, provided all
record types are assumed to have a ``Generic`` instance.  However, this does not
allow for the scope of fields to be controlled, and is likely to be less
efficient than providing built-in support for ``FieldType``.

Strictly speaking the restriction to boxed types is probably unnecessary,
because we could define::

  type family FieldRep  (x :: Symbol) (r :: Type) :: RuntimeRep
  type family FieldType (x :: Symbol) (r :: Type) :: TYPE (FieldRep x r)

This seems unreasonably complex, however.


Type-changing update
~~~~~~~~~~~~~~~~~~~~
A traditional ``Haskell2010`` record update such as ``t { foo = e }`` is able to
change the type of the field being updated, and hence the type of the record as
a whole.  For example::

  data T a = MkT { foo :: a }

  typeChangingUpdate :: T () -> T Bool
  typeChangingUpdate t = t { foo = True }

`Proposal #158 <https://github.com/ghc-proposals/ghc-proposals/pull/158>`_ does
not permit such type-changing updates, because defines a setter operation
``setField :: HasField x r a => r -> a -> r`` where the input and output record
types must both be ``r``.  This has the significant merit of simplicity, because
type inference has more information to work with, and there is no need to
specify under which circumstances type-changing updates are allowed.

However, type-changing updates are desirable for optics libraries, as the
restriction to non-type-changing update would mean they are unable to switch to
using ``HasField`` without loss of functionality.  Such a switch is desirable
for optics libraries because at the moment users must either (a) define lenses
for fields manually, (b) use Template Haskell which causes difficulties for
cross-compilation and compile-time performance issues, or (c) use generic
programming which imposes compile-time and runtime performance limitations.

In the light of this, we propose adding support for type-changing update to the
``GHC.Records`` API.  In particular, ``GHC.Records`` will expose both a function
``setFieldPoly`` that permits type-changing update and a function ``setField``
that specialises it to the case when type-changing update is not available::

  class SetFieldPoly x s t a b | x s -> a l, x t -> b l, x s b -> t, x t a -> s where
    setFieldPoly :: s -> b -> t

  type SetField x r a = SetFieldPoly x r r a a

  setField :: forall x r a . SetField x r a => r -> a -> r
  setField = setFieldPoly @x

In accordance with `proposal #282
<https://github.com/ghc-proposals/ghc-proposals/pull/282>`_, the
``RecordDotSyntax`` extension will continue to use ``setField`` and hence
**not** permit type-changing updates, i.e. turning on ``RecordDotSyntax`` would
cause the definition of ``typeChangingUpdate`` above to be rejected.


Modifiable parameters
^^^^^^^^^^^^^^^^^^^^^
TODO: explain: a parameter cannot be modified if it occurs in a different field.


Phantom parameters
^^^^^^^^^^^^^^^^^^
A phantom parameter is a type parameter of a datatype declaration that does not
occur in the type of any of its fields, for example ``s`` is phantom in::

  data Tagged s b = Tagged { unTagged :: b }

A traditional Haskell record update allows phantom parameters to be changed, so
for example the following is accepted::

  \x -> x { unTagged = unTagged x } :: Tagged s1 b -> Tagged s2 b

(Empty record updates are disallowed, so ``\x -> x {}`` cannot be used to change
phantom parameters without updating at least one field.)

Thus the question arises as to whether a type-changing update via
``setFieldPoly`` should be able to change a phantom parameter, i.e.  whether a
constraint such as ``SetFieldPoly "unTagged" (Tagged s1 a) (Tagged s2 b) a b``
should be solvable.

It is technically possible to solve such constraints, at least in current GHC
versions.  However, doing so violates the functional dependencies ``x s b -> t``
and ``x t a -> s`` in the definition of ``SetFieldPoly``.  This leads to a
failure to infer principal types.  For example, the following definition is
inferred to have the first type, but the second type is more general (and is
accepted with a type signature)::

  -- notPrincipal :: SetFieldPoly "foo" s t a b => s -> b -> (t, t)
  -- notPrincipal :: (SetFieldPoly "foo" s t a b, SetFieldPoly "foo" s t' a b) => s -> b -> (t, t')
  notPrincipal r v = (setFieldPoly @"foo" r v, setFieldPoly @"foo" r v)

Moreover, in some use cases for phantom parameters, it is intended that only
trusted code modifies the parameter.  This is typically enforced at module
boundaries by hiding the data constructor, but as the example above
demonstrates, it is also necessary to hide any fields.  This seems undesirable,
as it may not be obvious to users that merely exporting a field allows any
phantom parameters to be changed arbitrarily.

Thus we propose that the constraint solver should not allow ``SetFieldPoly``
constraints to change phantom parameters.  In cases where this is necessary, the
user can write a function that pattern matches on the data constructor (provided
it is in scope!).


Type parameters occurring under type families
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
TODO: explain: violation of functional dependency.

  type family F (x :: Type) :: Type
  type instance F Int  = Int
  type instance F Bool = Bool
  type instance F Char = Bool

  data UnderFamily c = MkUnderFamily { foo :: F c }



Higher-rank fields
~~~~~~~~~~~~~~~~~~
TODO: explain: in principle ``GetField`` is possible in some cases, but ``SetField`` is not.
Propose not to attempt them.


Partial fields
~~~~~~~~~~~~~~
In ``Haskell2010`` it is permitted to define *partial fields*, i.e. fields that
do not belong to every constructor of the datatype.  This means that traditional
record selection and update may throw runtime exceptions, as in these examples::

  data T = MkT1 { partial :: Int } | MkT2

  t = MkT2
  oops1 = partial t
  oops2 = t { partial = 0 }

Optics libraries in principle have a better story to tell here. Partial fields
give rise to *affine traversals*, where the accessor function returns a
``Maybe`` value and the setter leaves the value unchanged if it does not mention
the field (rather than throwing a runtime exception).

We could consider supporting this using built-in classes like the following::

  class HasPartialField x r a | x r -> a where
    getPartialField :: r -> Maybe a

  class SetPartialField x s t b | x t -> b, x s b -> t where
    setPartialField :: s -> b -> t

  type family FieldTotal x (r :: Type) :: Bool

Note that ``setField`` and ``setPartialField`` have the same type, but
``setField`` throws an exception on missing fields, whereas ``setPartialField``
returns the value unchanged.

For now we propose not to include such support for partial fields, although it
might be a useful future extension.  Alternatively, partial fields may be ruled
out at definition sites via the existing ``-Wpartial-fields``, or ruled out at
use sites via `proposed new warnings
<https://gitlab.haskell.org/ghc/ghc/-/issues/18650>`_.


Kind of field labels
~~~~~~~~~~~~~~~~~~~~
When ``HasField`` was originally introduced in `proposals #6
<https://github.com/ghc-proposals/ghc-proposals/pull/6>`_, the kind of the
parameter ``x`` representing the field label was polymorphic::

  class HasField (x :: k) r a | x r -> a where ...

While the class allows ``k :: Type`` to vary freely, ``HasField`` constraints
will be solved only if it is instantiated to ``Symbol``.  Moreover,
``RecordDotSyntax`` and approaches based on ``OverloadedLabels`` will only ever
generate constraints using ``Symbol``.  Other possibilities were originally
permitted in order to support hypothetical anonymous records libraries, which
might support different kinds of fields, e.g. drawn from explicitly-defined
enumerations.

The adjustment proposed to ``HasField`` in `proposals #158
<https://github.com/ghc-proposals/ghc-proposals/pull/158>`_ is not explicit
about whether such kind polymorphism should be present. It gives the class
signature as::

  class HasField x r a | x r -> a where ...

which is poly-kinded in ``x`` iff the ``PolyKinds`` extension is enabled.

The ``records-hasfield`` library makes use of the possibility to define label
kinds other than ``Symbol``, allowing tuples of labels to be used for
composition of fields.  For example, it defines an instance like::

  instance (HasField x1 r1 r2, HasField x2 r2 a2) => HasField '(x1, x2) r1 a2

This makes it slightly more convenient to define ``record-dot-preprocessor``,
but does not appear to be essential.

In the interests of simplicity, given the absence of a compelling known use
case, and a workaround described below, we propose to remove the kind
polymorphism.  That is, the classes will constrain the kind of the field label
parameter to be ``Symbol``.

In order to work around this (e.g. in an anonymous records library), one can
define a more polymorphic class ``HF`` as follows::

  type HF :: forall {k} . forall (x :: k) -> Type -> Type -> Constraint
  class HF x r a | x r -> a where ...
  instance {-# OVERLAPPABLE #-} HasField x r a => HF (x :: Symbol) r a where ...

The use of ``OVERLAPPABLE`` means that where the field label kind is determined
to be ``Symbol``, the instance will be selected and ``HasField`` from
``GHC.Records`` will be used, but instances can also be provided for other field
kinds.


Unlifted fields
~~~~~~~~~~~~~~~
The existing definition of ``HasField`` does not support unlifted fields, such
as in the following example::

  data T = MkT { foo :: Int# }

The constraint ``HasField "foo" T Int#`` is not even well-kinded, because the
field type is required to be a (lifted) type.

At the time ``HasField`` was introduced, it was not possible to define type
classes over potentially unlifted types.  However, thanks to levity polymorphism
in more recent GHC versions, this is now relatively straightforward.  In
particular, we can define::

  type HasField :: forall {l :: RuntimeRep} . Symbol -> Type -> TYPE l -> Constraint
  class HasField x r a | x r -> a where
    -- | Selector function to extract the field from the record.
    getField :: r -> a

This makes it possible to formulate and solve constraints such as ``HasField
"foo" T Int#``.

Observe that the parameter ``l :: RuntimeRep`` is inferred rather than specified
(hence the curly braces in the kind signature).  This means that when
``getField`` is used with explicit type application, the ``RuntimeRep``
parameter is skipped.


Visible foralls
~~~~~~~~~~~~~~~
At the time of writing, GHC supports "visible foralls" (visible dependent
quantification) in kinds, but not in the types of terms.  `Proposal #281
<https://github.com/ghc-proposals/ghc-proposals/pull/281>`_ proposes allowing
the types of terms to use visible foralls.  This is desirable for ``getField``
and similar functions, because it is always necessary to supply the field name
using a type application.

We currently have::

  getField :: forall (x :: Symbol) r a . HasField x r a => r -> a

which at use sites must use an explicit type application, e.g. ``getField
@"foo"``.  If the type application is omitted, an ambiguity error will result,
because there is no way to infer the field label from the record type or field
type.

If and when support for visible foralls is added, the type of ``getField`` could
change to::

  getField :: forall r a . forall (x :: Symbol) -> r -> a

meaning that we could instead use ``getField "foo"`` at use sites.  (Per the
visible forall proposal, here ``"foo"`` is a type-level ``Symbol`` even though
it syntactically resembles a ``String`` literal.)

This would be a breaking change, but since most user code is not expected to
call ``getField`` directly, and the use of a visible forall is strongly
preferable, we propose to permit changing the types of ``getField``,
``setField`` and ``setFieldPoly`` to use visible dependent quantification if and
when this is supported by GHC.


Multiple updates
~~~~~~~~~~~~~~~~
A traditional ``Haskell2010`` record update may change multiple fields
simultaneously, which may be important when types change.  For example::

  data Pair a = MkPair { first :: a, second :: a }

  multipleUpdate :: Pair Int -> Pair String
  multipleUpdate x = x { first = show (first x + second x), second = "" }

Here it is crucial that both fields are changed simultaneously, because ``Pair``
requires both its components to have the same types.

We do not currently have a good way to support such updates without introducing
significant complexity.  Thus if ``HasField`` is used, updates always apply one
at a time, and the definition of ``multipleUpdate`` will be rejected under
``RecordDotSyntax``.

This is not a drastic limitation because it is usually possible to generalise
the record type involved so that each field has an independent type, for example
by defining::

  type Pair a = Pair' a a
  data Pair' a b = MkPair { first :: a, second :: b }

Now the following alternate definition is accepted, including a subexpression
whose type is ``Pair' String Int``::

  multipleUpdate :: Pair Int -> Pair String
  multipleUpdate x = (x { first = show (first x + second x) }) { second = "" }



Proposed Change Specification
-----------------------------

When this proposal is implemented, the ``GHC.Records`` module will be defined as
follows::

  {-# LANGUAGE AllowAmbiguousTypes #-}
  {-# LANGUAGE ConstraintKinds #-}
  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE FlexibleInstances #-}
  {-# LANGUAGE FunctionalDependencies #-}
  {-# LANGUAGE PolyKinds #-}
  {-# LANGUAGE ScopedTypeVariables #-}
  {-# LANGUAGE StandaloneKindSignatures #-}
  {-# LANGUAGE TypeApplications #-}
  {-# LANGUAGE TypeFamilies #-}
  {-# LANGUAGE UndecidableInstances #-}

  module GHC.Records where

  import GHC.Types (Constraint, Symbol, Type, TYPE)

  -- | Constraint representing the fact that a field @x@ of type @a@ can be
  -- selected from the record type @r@.
  --
  -- This will be solved automatically for built-in records where the field is
  -- in scope, but manual instances may be provided as well.
  --
  type GetField :: forall {l} . Symbol -> Type -> TYPE l -> Constraint
  class GetField x r (a :: TYPE l) | x r -> a l where
    -- | Selector function to extract the field from the record.
    getField :: r -> a

  -- | Constraint representing the fact that a field @x@ of type @a@ can be
  -- updated in the record type @s@, producing a record of type @t@.
  --
  -- This will be solved automatically for built-in records where the field is
  -- in scope, but manual instances may be provided as well.
  --
  type SetFieldPoly :: forall {l} . Symbol -> Type -> Type -> TYPE l -> TYPE l -> Constraint
  class SetFieldPoly x s t a (b :: TYPE l) | x s -> a l, x t -> b l, x s b -> t, x t a -> s where
    -- | Update function to set the field @x@ in the record @s@.  Permits
    -- type-changing update.
    setFieldPoly :: s -> b -> t

  -- | Constraint representing the fact that a field @x@ of type @a@ can be
  -- selected from the record type @r@.
  type SetField :: forall {l} . Symbol -> Type -> TYPE l -> Constraint
  type SetField x r a = SetFieldPoly x r r a a

  -- | Update function to set the field @x@ in the record @r@.  Does not permit
  -- type-changing update.
  setField :: forall {l} x r (a :: TYPE l)  . SetField x r a => r -> a -> r
  setField = setFieldPoly @x

  -- | Constraint representing the fact that a field @x@ of type @a@ can be
  --  selected from or updated in the record @r@.
  type HasField :: forall {l} . Symbol -> Type -> TYPE l -> Constraint
  type HasField x r a = (GetField x r a, SetField x r a)

  -- | Constraint representing the fact that a field @x@ of type @a@ can be
  -- selected from the record @s@, or updated with a value of type @b@ to
  -- produce a record of type @t@.
  type HasFieldPoly :: forall {l} . Symbol -> Type -> Type -> TYPE l -> TYPE l -> Constraint
  type HasFieldPoly x s t a b = (GetField x s a, SetFieldPoly x s t a b)

  -- | If there is a field @x@ in the record type @r@, returns the type of the
  -- field.  The field must have a simple type of kind 'Type' (i.e. it may not
  -- be higher-rank, existential or unboxed).
  type family FieldType (x :: Symbol) (r :: Type) :: Type


To summarise the changes:

* The ``HasField`` class has been renamed to ``GetField``.  In its place there
  is a new ``HasField`` constraint synonym for the pair of constraints
  ``GetField`` and ``SetField``.

* ``SetField`` is now a constraint synonym for ``SetFieldPoly``, a new class
  that permits type-changing update.

* The classes are polymorphic in the runtime representation of the field type,
  allowing support for `Unlifted fields`_. Standalone kind signatures and
  explicit specificity annotations are used to make this polymorphism explicit.

* The classes are no longer polymorphic in the kind of field labels. This is now
  restricted to be ``Symbol``.

* A new ``FieldType`` type family makes it possible to look up the type of a
  field.


Solving ``GetField`` constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The following is a specification of constraint solving behaviour for
``GetField``.  This is essentially unchanged from the solving behaviour for
``HasField`` in existing GHC versions, which is described in the `GHC user's
guide <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/hasfield.html#solving-hasfield-constraints>`_.

A wanted constraint ``GetField f r a`` will be solved automatically by
GHC's constraint solver when the following hold:

* ``f`` is a type-level symbol ``"foo"``.

* ``r`` is an application of a record type ``R`` to some arguments ``t0 ... tn``.

* The record type ``R x0 ... xn`` has a field ``foo`` (of some type ``u[x0,...,xn]``).

* The field ``foo`` is in scope, according to the usual module scope rules.

* The field type ``u[x0,...,xn]`` does not refer to any existentially-quantified
  type variables or contain any universal quantifiers.

In this case, the constraint solver will discharge the original constraint, and
emit new constraints:

* ``a ~ u[t0/x0,...,tn/xn]`` (equating the type from the wanted with the actual
  type of the field);

* TODO: something about GADTs;

* any constraints from the datatype context (defined with ``DatatypeContexts``),
  if there is one.

Note that:

* If ``R`` is a data family, it is considered a record type iff there is an
  instance of the family for ``R t0 ... tn`` that is defined as a record.

* Solving the equation between the wanted and actual field types will fill in
  the inferred parameter ``l :: RuntimeRep`` with the appropriate
  representation.  This means support for unlifted fields is automatic.


Solving ``SetFieldPoly`` constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general, the constraint solving behaviour for ``SetFieldPoly`` is slightly
more complex than ``GetField``, because of the possibility of type-changing
updates.  When the original and updated record types are the same (e.g. the
``SetField`` constraint synonym is used), then the following rules specialise to
the rules for ``GetField``.  That is, a constraint ``SetFieldPoly f r r a`` will
be solved automatically iff ``GetField f r a`` is solved automatically.
(TODO: verify this claim.)

A wanted constraint ``SetFieldPoly f s t a b`` will be solved automatically by
GHC's constraint solver when the following hold:

* ``f`` is a type-level symbol ``"foo"``.

* At least one of ``s`` or ``t`` is an application of a record type ``R`` to
  some arguments ``t_0 ... t_n``.

* The record type ``R x_0 ... x_n`` has a field ``foo`` (of some type ``u[x_0, ..., x_n]``).

* The field ``foo`` is in scope, according to the usual module scope rules.

* The field type ``u[x_0, ..., x_n]`` does not refer to any existentially-quantified
  type variables or contain any universal quantifiers.

Definition: a type parameter ``x_i`` of the record type ``R x_0 ... x_n`` is
*modifiable* if:

 * it occurs in the type ``u[x0, ..., xn]`` of the field ``foo``;

 * at least one occurrence is not under a type family; (TODO: define more precisely)

 * it does not occur in the type of any other field.

Suppose without loss of generality that ``t = R t_0 ... t_n`` (otherwise
interchange ``s`` and ``t``, noting that if both ``s`` and ``t`` are already
applications of ``R`` then the constraints are equivalent in either order).

In this case, the constraint solver will discharge the original constraint, and
emit new constraints as follows.

* ``s ~ R s_0 ... s_n`` where ``s_i = alpha_i`` for a fresh unification variable
  ``alpha_i`` if ``x_i`` is modifiable, or ``s_i = t_i`` otherwise;

* ``a ~ u[s_0/x_0, ..., s_n/x_n]``;

* ``b ~ u[t_0/x_0, ..., t_n/x_n]``;

* TODO: something about GADTs;

* any constraints from the datatype context (defined with ``DatatypeContexts``),
  if there is one.

TODO: explore DYSFUNCTIONAL implications... are we violating the fundep? does it matter?

TODO: explain when manual SetField instances are permitted?


Reducing the ``FieldType`` type family
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TODO: specify


Flags for compile-time performance control
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An "updater function" for a field is a function that takes a record value and a
new value for a field, and returns the result of setting the field to the value.
For example, given a field ``foo :: A`` in a record type ``T``, the updater
function for ``foo`` is the function::

  upd :: T -> A -> T
  upd t a = t { foo = a } -- using normal Haskell2010 record update syntax

Notice that such a function corresponds precisely to the dictionary of a
``SetField "foo" T A`` constraint.  Thus for the constraint solver to solve a
``SetField`` constraint automatically, it must produce an updater function,
either by generating them at field definition sites or on-the-fly at use sites.
(Updater functions are produced internally by GHC; they cannot be referenced
directly in user code, because their names are not in scope.)

When a module defines large record types, the compile-time cost of generating
updater functions up front at datatype definition sites becomes significant (see
the `implementation of proposal #158
<https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3257>`_).  In a code base
that makes infrequent use of ``setField`` (or mechanisms that depend upon it,
such as record updates under ``RecordDotSyntax``), it is not desirable to pay
this cost for up front compilation of updaters.  Instead, by default GHC should
solve ``SetField`` constraints by generating an updater function on-the-fly.

On the other hand, code bases making substantial use of ``setField`` may benefit
from generating updater functions in advance, because work will be saved at use
sites.

To address this, we propose a new compiler flag, ``-fgenerate-record-updaters``,
with the following behaviour:

* with ``-fno-generate-record-updaters`` (the default), record updaters will not
  be generated in advance and GHC will correspondingly perform more work when
  solving ``SetField`` constraints;

* with ``-fgenerate-record-updaters`` set, record updaters will be generated at
  datatype definition sites and solving ``SetField`` constraints will be
  correspondingly cheaper.

This flag is merely compile-time performance optimizations. It has no effect on
which programs type-check.  There is no way to specify different values of the
flags for multiple datatypes in a single module.

It should be possible for a user compiling an application to set the flag at
build time even if the original author of a library being compiled did not
consider the need for the flag; thus we do not require a pragma in the source
file containing the datatype definition.



Examples
--------
This section illustrates the specification through the use of examples of the
language change proposed. It is best to exemplify each point made in the
specification, though perhaps one example can cover several points. Contrived
examples are OK here. If the Motivation section describes something that is
hard to do without this proposal, this is a good place to show how easy that
thing is to do with the proposal.

TODO: examples of solving!


Effect and Interactions
-----------------------

``NoFieldSelectors`` and ``-fno-generate-record-selectors``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The accepted `proposal #160
<https://github.com/ghc-proposals/ghc-proposals/pull/160>`_ defined a new
language extension ``NoFieldSelectors``, which prevents field selector functions
being in scope within expressions.  Fields can still be used in record syntax
(construction, pattern-matching and update) and with ``HasField``.  This
extension is `implemented
<https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4743>`_ and should be
available in GHC 9.2.

``NoFieldSelectors`` permits top-level definitions whose names would otherwise
conflict with fields defined in the same module, and in particular, means lenses
can be defined using the same names as the fields.

By default, even when ``NoFieldSelectors`` is in use, GHC will internally
generate selector functions at field definition sites, so they are available for
use with ``GetField``.  The ``NoFieldSelectors`` language extension controls
whether selector functions are in scope, but not whether the compiler generates
them at all.

However, as with updaters, compiling these selector functions is somewhat
expensive for large record types, because the cost is quadratic in the number of
fields (every field has a selector, and every selector includes a case
expression that binds all the fields, even though all but one are unused).  Thus
in programs that define very large record types, but rarely use selector
functions, it would be helpful to remove the up-front cost.

Hence we also propose a new compiler flag, ``-fgenerate-record-selectors``, with
the following behaviour:

* with ``-fgenerate-record-selectors`` (the default), record selectors will be
  generated at datatype definition sites and solving ``GetField`` constraints
  will be correspondingly cheaper;

* with ``-fno-generate-record-selectors``, record selectors will not be
  generated in advance and GHC will correspondingly perform more work when
  solving ``GetField`` constraints.

It is an error to use ``-fno-generate-record-selectors`` if
``-XNoFieldSelectors`` is not also set.

It may be possible to reduce the compile-time cost of generating record
selector/updater functions in large record types, which would reduce the
motivation for the ``-fno-generate-record-selectors`` and
``-fno-generate-record-updaters`` options.  However it is not clear how to go
about this, as it requires new primitives and/or changes to Core, GHC's typed
intermediate language.


Pattern synonyms
~~~~~~~~~~~~~~~~
The ``PatternSynonyms`` extension allows the definition of record pattern
synonyms, such as::

  pattern MyJust {theValue} = Just theValue

By default, ``theValue`` can be used as a (partial) record selector function of
type ``Maybe a -> a``, and can be used with record construction,
pattern-matching and update syntax, e.g. ``MyJust { theValue = 3 }`` means
``Just 3``.  This is helpful because if a record datatype definition changes,
pattern synonyms can be provided for compatibility purposes.

However, ``HasField`` constraint solving does not support such pattern synonyms,
e.g. a constraint like ``HasField "theValue" (Maybe Int) Int`` will not
automatically be solved.  This means that ``RecordDotSyntax`` and optics-based
approaches using ``HasField`` will expose the difference between a record
datatype and the corresponding pattern synonym.

A workaround for this exists in the form of "virtual record fields" given by
manual ``HasField`` instances.  For this example, the user could define an
(orphan) instance::

  instance a ~ b => GetField "theValue" (Maybe a) b where
    getField = theValue

For now we do not propose generating such instances automatically.  In
particular, this is complicated by the possibilities that pattern synonyms may
be defined independently of the underlying type (which would give rise to orphan
instances, as in the ``Maybe`` example), the type need not even be a record, and
multiple pattern synonyms may define conflicting fields for the same type.


Costs and Drawbacks
-------------------
This will require moderate development effort, as the current implementation of
``HasField`` constraint solving relies on generating selector/updater functions
up front, rather than constructing them during constraint solving as required by
``-fno-generate-record-updaters``.  It does not seem like it will introduce a
substantial maintenance burden.

Novice users may find ``HasField`` and ``RecordDotSyntax`` more complex to
reason about than traditional Haskell record syntax.  However this proposal has
taken care to ensure the more complex aspects (e.g. type-changing update) need
not be exposed to those who do not go looking for them.

For users who do not wish to use ``HasField`` at all, the approach taken in this
proposal should mean they do not pay a compile-time performance cost, and can
happily ignore the ``GHC.Records`` module and ``RecordDotSyntax`` extension.


Alternatives
------------
There are many alternative designs possible for ``HasField`` and related
classes, which is part of the reason progress in this area has been slow.  The
`Design questions`_ section above attempts a detailed discussion of each
individual design choice, but there are many minor variations possible.

* `Proposal #158 <https://github.com/ghc-proposals/ghc-proposals/pull/158>`_
  used a design with a single ``HasField`` class, no type-changing update,
  functional dependencies.  This is the current accepted design, although the
  implementation is not yet merged into GHC HEAD.

* `Proposal #286 <https://github.com/ghc-proposals/ghc-proposals/pull/286>`_
  suggests splitting ``HasField`` into two classes and switching to type
  families in place of functional dependencies.  It gives a rather larger
  definition for the ``SetField`` class, including ``GetField`` as a
  superclass.

* TODO: cite https://github.com/effectfully-ou/sketches/tree/master/has-lens-done-right

Another possible approach is to abandon ``HasField`` as a solution to the
"Records Problem" in Haskell.

* Optics libraries provide various options for working with record types, and
  they do not necessarily need ``HasField``, although some use cases could
  directly benefit from it.

* `Proposal #180 <https://github.com/ghc-proposals/ghc-proposals/pull/180>`_
  suggests adding support for row polymorphism in GHC.  However, this would
  require significant work to produce a full design, let alone an
  implementation, and that seems unlikely to happen in the near future.

* `Proposal #310 <https://github.com/ghc-proposals/ghc-proposals/pull/310>`_
  suggests adding a syntax for record update that would explicitly specify the
  type, thereby avoiding the need for type-directed field resolution.  However,
  this conflicts with the (accepted) ``RecordDotSyntax`` proposal.



Unresolved Questions
--------------------
``SetFieldPoly`` is a terrible name. What should it be called?

Are there other design choices surrounding ``HasField`` not yet considered here?

Does the ``FieldType`` type family pull its weight?  It is not necessary for
normal use of ``HasField``, and can be approximated using ``GHC.Generics``.


Implementation Plan
-------------------
The proposal author, Adam Gundry, will implement this change if accepted.  The
implementation of this proposal (or some other way to support ``setField``) is
currently blocking the full implementation of ``RecordDotSyntax`` (`proposal
#282 <https://github.com/ghc-proposals/ghc-proposals/pull/282>`_).


Endorsements
-------------
(Optional) This section provides an opportunty for any third parties to express their
support for the proposal, and to say why they would like to see it adopted.
It is not mandatory for have any endorsements at all, but the more substantial
the proposal is, the more desirable it is to offer evidence that there is
significant demand from the community.  This section is one way to provide
such evidence.
