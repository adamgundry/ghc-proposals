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
selection: given the expression ``getField @"foo" t``, the inferred type of
``t`` can be used to determine which ``foo`` field is meant, even if there are
multiple ``foo`` fields in scope and hence the expression ``foo t`` would be
ambiguous.  (This arises in particular with the ``DuplicateRecordFields``
extension, which has a somewhat ad hoc mechanism for disambiguating such
expressions that is to be removed following `proposal #366
<https://github.com/ghc-proposals/ghc-proposals/pull/366>`_.)

However, the status quo is lacking in two important respects:

1. There is no facility for updating fields, corresponding to record update
   syntax ``t { foo = v }`` in traditional Haskell.

2. The syntax ``getField @"foo"`` is rather convoluted.

As a result, ``HasField`` has seen relatively little use to date.  Several more
recent proposals have suggested changes to address this; they are recapitulated
in subsequent sections.  In particular, the accepted `proposal #158
<https://github.com/ghc-proposals/ghc-proposals/pull/158>`_ planned to change
the definition of ``HasField`` to support updates, and the accepted `proposal
#282 <https://github.com/ghc-proposals/ghc-proposals/pull/282>`_ (as modified by
`proposal #405 <https://github.com/ghc-proposals/ghc-proposals/pull/405>`_)
introduced new extensions to provide "record dot syntax".  In the light of
experience implementing these proposals, and discussion arising from `proposal
#405 <https://github.com/ghc-proposals/ghc-proposals/pull/405>`_, it seems worth
systematically re-evaluating the design choices surrounding ``HasField`` and
type-directed name resolution for field updates.


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


Recap: Record dot syntax
~~~~~~~~~~~~~~~~~~~~~~~~
The accepted `proposal #282
<https://github.com/ghc-proposals/ghc-proposals/pull/282>`_ (as modified by
`proposal #405 <https://github.com/ghc-proposals/ghc-proposals/pull/405>`_)
defined two new extensions for "record dot syntax":

* ``OverloadedRecordDot`` adds dot syntax for record selection, interpreted
  using ``getField``, e.g. ``t.foo`` translates to ``getField @"foo"``.

* ``OverloadedRecordUpdate`` changes the interpretation of the existing record
  update syntax to use ``setField``, e.g. ``t { foo = e }`` translates to
  ``setField @"foo" t e``.  This relies on the planned introduction of
  ``setField`` from `proposal #158
  <https://github.com/ghc-proposals/ghc-proposals/pull/158>`_.

Originally these were bundled together under one ``RecordDotSyntax`` extension,
but they were separated under `proposal #405
<https://github.com/ghc-proposals/ghc-proposals/pull/405>`_.  GHC 9.2 is
expected to have full support for ``OverloadedRecordDot``, but
``OverloadedRecordUpdate`` will not be fully implemented and will be regarded as
subject to change in subsequent releases.

A particular point of controversy is type-changing update.  The ``setField``
operation from `proposal #158
<https://github.com/ghc-proposals/ghc-proposals/pull/158>`_ does not allow
type-changing update, and since `proposal #282
<https://github.com/ghc-proposals/ghc-proposals/pull/282>`_ built upon it,
``RecordDotSyntax`` as originally accepted by the GHC Steering Committee did not
permit type-changing update.  However committee discussion on `proposal #405
<https://github.com/ghc-proposals/ghc-proposals/pull/405>`_ made it clear that
this question should be re-addressed.


Design aims and objectives
~~~~~~~~~~~~~~~~~~~~~~~~~~
The purposes of this proposal are:

* to articulate the various design choices to be made regarding the ``HasField``
  class, and agree and clearly specify a final design; and

* to seek agreement on whether type-changing update should be available under
  ``OverloadedRecordUpdate`` (as this question was reopened in the discussion on
  `proposal #405 <https://github.com/ghc-proposals/ghc-proposals/pull/405>`_).

In particular, we seek a design based on the following criteria:

* ``HasField`` should offer a foundation for both record dot syntax and
  optics-based approaches to record operations.

* As far as possible, type inference behaviour and type error messages directly
  related to ``HasField`` should be easy for users to understand.

* The compile-time performance cost for using ``HasField`` should be comparable
  to normal uses of record syntax. Implementing this proposal should impose no
  compile-time performance cost on code that does not use ``HasField``.

* The API provided by the ``GHC.Records`` module itself is not expected to be
  called directly by typical users.  Rather, this module should provide internal
  functionality for record dot syntax and for optics libraries.

The following are out of scope for this proposal:

* Support for anonymous records. There are many design choices around different
  ways to integrate anonymous records with Haskell, and the right way forward is
  not obvious. ``HasField`` should reflect the capabilities of existing Haskell
  records, and need not offer capabilities that are not useful in this context.
  (Thus the approach advocated here contrasts with e.g. the dormant `proposal
  #180 <https://github.com/ghc-proposals/ghc-proposals/pull/180>`_ which seeks
  to add support for row polymorphism.)

* Changes to syntax, which were discussed extensively in `proposal #282
  <https://github.com/ghc-proposals/ghc-proposals/pull/282>`_.

Now that we have established overall criteria for the design, the next
subsections will summarise the design recommended by this proposal, with a
particular focus on the topic of type-changing update.


Design highlights
~~~~~~~~~~~~~~~~~
The essence of the new design is captured in the following definitions, which
will replace the existing contents of ``GHC.Records``.  For a complete picture
of the new contents of this module, including auxiliary definitions, see the
`Proposed Change Specification`_.

::

  class GetField (x :: Symbol) r (a :: TYPE l) | x r -> a l where
    getField :: r -> a

  class SetFieldPoly (x :: Symbol) s t (b :: TYPE l) | x t -> b s l, x s -> t l where
    setFieldPoly :: b -> s -> t

  type SetField x r a = SetFieldPoly x r r a

  setField :: SetField x r a => a -> r -> r

These are the key points of the new design.  Detailed justification for each
point is deferred to subsequent sections.

* The existing ``HasField x r a`` class has been renamed to ``GetField x r
  a``. It continues to have a single ``getField :: r -> a`` method for record
  field selection.

* There is a new class ``SetFieldPoly x s t b``, with a single method
  ``setFieldPoly :: b -> s -> t`` that provides (type-changing) update of a
  single field.

* The ``setFieldPoly`` function takes the field value first, followed by the
  record value.

* The function ``setField`` is defined as a type-restricted version of
  ``setFieldPoly`` that does not allow type-changing update.

* The classes are polymorphic in the runtime representation of the field type,
  allowing support for `Unlifted fields`_.

* The classes are no longer polymorphic in the kind of field labels, which is
  now restricted to be ``Symbol``.

The main changes from the design in `proposal #158
  <https://github.com/ghc-proposals/ghc-proposals/pull/158>`_ are:

* The ``getField`` and ``setField`` operations belong to two independent
  classes, rather than both being definable from a single method in a single
  ``HasField`` class.

* Type-changing update is permitted when ``setFieldPoly`` is called (although
  not when ``setField`` is called).

* The order of arguments to ``setField`` is reversed.


Type-changing update
~~~~~~~~~~~~~~~~~~~~
A traditional ``Haskell2010`` record update such as ``t { foo = e }`` is able to
change the type of the field being updated, and hence the type of the record as
a whole.  For example::

  data T a = MkT { foo :: a }

  typeChangingUpdate :: T () -> T Bool
  typeChangingUpdate t = t { foo = True }

Type inference for such definitions is relatively unproblematic in traditional
Haskell, because the field name must uniquely determine the record type being
updated, or else the definition is rejected as ambiguous.  The situation is more
complex in the context of ``SetField``, where definitions may be polymorphic in
the record type to which they relate.

`Proposal #158 <https://github.com/ghc-proposals/ghc-proposals/pull/158>`_ does
not permit such type-changing updates, because it defines a setter operation
``setField :: HasField x r a => r -> a -> r`` where the input and output record
types must both be ``r``.  This has the significant merit of simplicity, because
type inference has more information to work with, and there is no need to
specify under which circumstances type-changing updates are allowed.

However, type-changing updates are desirable for libraries such as ``optics``.
Moreover, some people would prefer type-changing update to be supported by
record dot syntax, although this is controversial.

In the light of this, we propose adding support for type-changing update to the
``GHC.Records`` API.  In particular, ``GHC.Records`` will expose both a function
``setFieldPoly`` that permits type-changing update and a function ``setField``
that specialises it to the case when type-changing update is not available::

  class SetFieldPoly x s t b | ... where
    setFieldPoly :: b -> s -> t

  type SetField x r a = SetFieldPoly x r r a

  setField :: forall x r a . SetField x r a => a -> r -> r
  setField = setFieldPoly @x

Crucially, using the ``SetField`` constraint synonym or the ``setField``
function ensures that the record type cannot change, so type inference behaviour
should be exactly the same as if type-changing update were not available at all.
However, users who need type-changing update can use ``SetFieldPoly`` instead.

This leaves open two questions:

* How should type inference work for ``SetFieldPoly`` constraints?

* Should record update syntax permit type-changing update?


The challenge of type inference for ``SetFieldPoly`` constraints
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
For a ``GetField`` constraint (corresponding to a ``HasField`` constraint in
existing GHC versions), the constraint solver will automatically solve a
constraint like ``GetField "f" (T a b c) ty`` when ``T`` is a record datatype
with a field ``f`` in scope.  That is, given a constraint ``GetField x r a``
the ``x`` parameter must be a ``Symbol`` literal, the ``r`` parameter must be
a record type constructor (applied to some arguments), and the record must have
a field of the appropriate name.

Correspondingly, we expect a non-type changing ``SetField "f" (T a b c) ty``
constraint, which is equivalent to ``SetFieldPoly "f" (T a b c) (T a b c) ty``,
to be solved automatically in the same way.

However, this is not enough if we want to allow type-changing update.  For example::

  data T a = MkT { f :: a }

  fun :: T () -> T Int
  fun t = setFieldPoly @"f" 0 t
  -- constraint arising:  SetFieldPoly "f" (T ()) (T Int) alpha

In this case the constraints to solve are ``Num alpha`` and
``SetFieldPoly "f" (T ()) (T Int) alpha`` where ``alpha`` is a unification
variable representing the type of the numeric literal ``0``.  Here the
``SetFieldPoly`` constraint is easily solved as we do not require the type
parameters for the two occurrences of ``T`` to be the same, and we do not need
the field type to be determined.  Instead, we can see that the record type being
updated is ``T``, and infer that the field type ``alpha`` from the constraint
must match the actual type of the ``f`` field of ``T Int``, namely ``Int``.

More interesting cases arise if we have partial type information::

  fun2 t = setFieldPoly @"f" 0 (t :: T ())
  -- interim inferred type:  T () -> beta
  -- constraint arising:  SetFieldPoly "f" (T ()) beta alpha  (Num alpha)
  -- final inferred type:  Num a => T () -> T a

  fun3 t = (setFieldPoly @"f" 0 t) :: T Int
  -- interim inferred type:  gamma -> T Int
  -- constraint arising:  SetFieldPoly "f" gamma (T Int) alpha  (Num alpha)
  -- final inferred type:  T a -> T Int

In each case the comment shows the ``SetFieldPoly`` constraint that arises.  We
can handle these constraints too, by exploiting the fact that type-changing
update does not change the choice of record type constructor, merely its
parameters.  Thus if *either* the ``s`` or ``t`` parameters is a concrete record
type, we can infer that the other parameter must be some instance of the same
record type/ For example, in the ``fun2`` case we infer that ``beta ~ T alpha1``
for some fresh unification variable ``alpha1``, then unify the types for the
field to get ``alpha ~ alpha1``.

On the other hand, if neither record parameter is a concrete record type, we
cannot determine the record type and solve the ``SetFieldPoly`` constraint but
must generalise over it in the usual way::

  fun4 t = setFieldPoly @"f" 0 t
  -- interim inferred type:  delta -> epsilon
  -- constraint arising:  SetFieldPoly "f" delta epsilon alpha
  -- final inferred type:  (Num b, SetFieldPoly "f" s t b) => s -> t

To recap, we have seen that it is unproblematic to support type-changing update
where the record type is concrete (either before or after the update), and that
simple cases of polymorphic updates are possible.

However, things become more difficult if we try to *compose* polymorphic
updates.  For example::

  fun5 = setFieldPoly "g" True . setFieldPoly "f" ()
  -- interim inferred type: beta -> delta
  -- constraints arising:  SetFieldPoly "f" beta gamma ()
  --                       SetFieldPoly "g" gamma delta Bool
  -- final inferred type:  (SetFieldPoly "f" s t (), SetFieldPoly "g" t u Bool) => s -> u

Here we have an ambiguity problem: the type variable ``t`` is ambiguous, because
it appears only in the context to the left of the ``=>`` sign.  But rejecting
this definition would be distinctly unsatisfactory, because it is perfectly
possible to call ``fun5`` unambiguously: in a context that fixes ``s`` or ``u``
to be a concrete record type with ``f`` and ``g`` fields, the ``SetFieldPoly``
constraints will become solvable, and will determine the middle type ``t``
automatically.

Functional dependencies to the rescue?
""""""""""""""""""""""""""""""""""""""

The usual solution to such ambiguity problems would be to introduce functional
dependencies between the parameters of the typeclass, e.g. previous designs for
type-changing update have used something like::

  class SetFieldPoly x s t b | x s b -> t where
    setFieldPoly :: b -> s -> t

Here the functional dependency ``x s b -> t`` asserts that the field name ``x``,
input record type ``s`` and new field type ``b`` can be used to determine the
output record type ``t``.  This would mean ``fun5`` was accepted without
ambiguity, because the functional dependency can be used to determine ``t`` from
``"f"``, ``s`` and ``()`` in ``SetFieldPoly "f" s t ()``.

Unfortunately, this functional dependency is not sufficient to handle the
following example, where the field types are not uniquely determined, so ``t``,
``a`` and ``b`` are all ambiguous::

  fun6 = setFieldPoly @"k" 0 . setFieldPoly @"h" []
  -- interim inferred type: beta -> delta
  -- constraints arising:  SetFieldPoly "h" beta gamma [alpha]
  --                       SetFieldPoly "k" gamma delta epsilon  (Num epsilon)
  -- final inferred type:  (Num b, SetFieldPoly "h" s t [a], SetFieldPoly "k" t u b) => s -> u

Nor can it handle examples where inference needs to proceed "in reverse" from
the result type of the update to the type being updated, e.g. here ``s`` is
ambiguous::

  fun7 = setFieldPoly @"l" () undefined
  -- interim inferred type: gamma
  -- constraints arising: SetFieldPoly "l" beta gamma ()
  -- final inferred type:  SetFieldPoly "l" s t () => t

Not only does the functional dependency ``x s b -> t`` fail to determine enough
type variables unambiguously, but also it is too restrictive, because it rules
out certain type-changing updates that are accepted by traditional Haskell
record updates.  For example, this arises with phantom type parameters::

  data Tagged u w = Tagged { unTagged :: w }

  phantomTypeChangingUpdate1 x = x { unTagged = unTagged x }
  -- inferred type: Tagged u w -> Tagged v w
  -- with traditional Haskell records

  phantomTypeChangingUpdate2 x = setFieldPoly @"unTagged" (unTagged x) x
  -- interim inferred type: Tagged u beta -> gamma
  -- constraints arising:  SetFieldPoly "unTagged" (Tagged u beta) gamma beta
  -- final inferred type: SetFieldPoly "unTagged" (Tagged u w) (Tagged v w) w => Tagged u w -> Tagged v w

Here we need to solve a constraint where the record type is known, but solving
it would violate the ``x s b -> t`` functional dependency, because ``t = Tagged
v w`` has an occurrence of ``v`` that is not determined by ``x = "unTagged"``,
``s = Tagged u w``, ``b = w``.



Dysfunctional dependencies to the rescue
""""""""""""""""""""""""""""""""""""""""
Consider instead the following definition::

  class SetFieldPoly x s t b | x s -> t, x t -> s b where
    setFieldPoly :: b -> s -> t

At first glance, this is somewhat surprising. It claims that if we know the
field name ``x``, then knowledge of either ``s`` or ``t`` will allow the other
type to be determined, regardless of the field type.  Morever, the field type
``b`` can be determined from ``x`` and ``t`` (and hence ``x`` and ``s``).  The
strong functional dependencies mean that even examples like ``fun6`` are no
problem, because there is no ambiguity.

But does this declaration even make sense? Any type-changing update will violate
the functional dependency.  For example, GHC would not even allow us to define::

  instance SetFieldPoly "unTagged" (Tagged s a) (Tagged t b) b

because it violates the liberal coverage condition.  So what goes wrong if we
allow the constraint solver to solve such "dysfunctional" constraints anyway?

First, some terminology:

* **Termination**
    The constraint solving process finishes with a result in finite time.

* **Confluence**
    If a set of constraints A can be simplified in two different ways to B or C,
    then there must be a common set of constraints D such that both B and C can
    be simplified to D.  This implies that the result of constraint solving does
    not depend on the order in which constraints are tackled by the algorithm.

* **Coherence**
    Every possible solution to a set of constraints leads to the same runtime
    behaviour of the programme.

* **Consistency**
    There is no way to solve a constraint that entails an equality between two
    distinct types, e.g. ``Int ~ Bool``.  This is an essential prerequisite for
    **type soundness**.  Modulo bugs and explicitly unsafe features such as
    ``unsafeCoerce``, GHC never allows consistency to be violated, and indeed
    the constraint solver goes to some trouble to generate evidence that can be
    checked by Core Lint, precisely to avoid inconsistency.

The proposed "dysfunctional" behaviour should not affect consistency.  This is
because functional dependencies do not carry evidence, i.e. even if we know both
``[G] SetFieldPoly "x" s t ()`` and ``[G] SetFieldPoly "x" s u ()``, there is no
way to conclude ``t ~ u``.  Instead, the functional dependencies work more like
hints to the constraint solver: if it knows ``[G] SetFieldPoly "x" s t ()`` and
is solving ``[W] SetFieldPoly "x" s u ()``, then it will try to solve ``[W] t ~
u``.

(Arguably it might be better if we had two separate features: true
evidence-carrying functional dependencies, and some kind of more flexible "type
inference hints" that could be used in the ``SetFieldPoly`` case.  See
discussion in this direction on `ghc-proposals#374
<https://github.com/ghc-proposals/ghc-proposals/pull/374>`_ and `ghc-proposals
issue #391 <https://github.com/ghc-proposals/ghc-proposals/issues/391>`_.  But
for the moment, users requiring true functional dependencies can encode them
with type families, while those looking to give hints to the constraint solver
already routinely (ab)use functional dependencies for this purpose.)

In principle "dysfunctional dependencies" break confluence, however, and hence
potentially coherence.  This is difficult to observe in practice, however.
(TODO: would be nice to have a concrete example?)  But GHC's constraint solver
is known to be non-confluent already (`#10675
<https://gitlab.haskell.org/ghc/ghc/-/issues/10675>`_, `#18851
<https://gitlab.haskell.org/ghc/ghc/-/issues/18851>`_) and the sky has not
fallen in.  While users can discover confusing behaviour arising from
non-confluence or incoherence if they try hard enough, it is not usually a
problem that they stumble over accidentally.


One unexpected consequence of this approach that users may encounter is that
making type-changing updates to the same field more than once in a single
definition may result in a type that is overly specific.  For example::

  hmm v r = (setFieldPoly @"foo" v r, setFieldPoly @"foo" v r)
  -- interim inferred type: alpha -> beta -> (gamma, delta)
  -- constraints arising:  SetFieldPoly "foo" beta gamma alpha
  --                       SetFieldPoly "foo" beta delta alpha
  -- final inferred type:  SetFieldPoly "foo" s t b => b -> s -> (t, t)
  -- most general type:    (SetFieldPoly "foo" s t b, SetFieldPoly "foo" s t' b) => b -> s -> (t, t')

Here the two wanted constraints lead to a functional dependency improvement
``gamma ~ delta``. According to a strict reading of the functional dependency,
the "most general" type is equivalent to the inferred type.  However, if
"dysfunctional" solutions are allowed, the two types are distinguishable.



Should ``OverloadedRecordUpdate`` use type-changing update?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The original plan for the ``OverloadedRecordUpdate`` extension (`proposal #282
<https://github.com/ghc-proposals/ghc-proposals/pull/282>`_ and `proposal #405
<https://github.com/ghc-proposals/ghc-proposals/pull/405>`_) was that it would
**not** permit type-changing updates, i.e. it would use ``setField`` rather than
``setFieldPoly`` (in the language of the current proposal).  Thus, turning on
``OverloadedRecordUpdate`` would cause the definition of ``typeChangingUpdate``
above to be rejected, which is unfortunate.

Opinion is divided as to how important type-changing update is, with some people
willing to give it up and others concerned about its loss.  Thus we can consider
several alternative possibilities:

* Translate ``OverloadedRecordUpdate`` using ``setField`` so it is not type-changing.
    This is simple but restrictive.  It means that enabling
    ``OverloadedRecordUpdate`` will break existing code that uses type-changing
    updates.  It is still useful to have ``setFieldPoly`` available for optics
    libraries.

* Translate ``OverloadedRecordUpdate`` using ``setFieldPoly`` so it allows (some) type-changing updates .
    This means users need to understand the rules around when ``SetFieldPoly``
    constraints will be solved.  As the discussion above indicates, these rules
    will be nontrivial.  This will still not be completely backwards compatible
    as some type-changing updates permitted in traditional Haskell record update
    cannot be supported be ``SetFieldPoly``.

* Introduce new syntax to distinguish type-changing from non-type-changing updates.

* Introduce new syntax for performing an update while specifying the type being updated.
    See `proposal #310 <https://github.com/ghc-proposals/ghc-proposals/pull/310>`_.
    This is comparable to the ``DisambiguateRecordFields`` extension, which uses
    the data constructor in a record construction or pattern match to determine
    the type without need for type-directed field resolution.  This would make
    it possible to write type-changing updates (or other updates not supported
    by ``SetFieldPoly``), but would not allow overloading.

In any case, users can choose to enable ``OverloadedRecordDot`` without
``OverloadedRecordUpdate``, meaning that dot notation for selection is
available, while updates are still treated in the traditional manner and may be
type-changing but not overloaded.  Users may also write out type-changing
updates explicitly (e.g. replacing ``t { foo = True }`` with ``case t of MkT{..}
-> MkT{foo=True, ..}``) or use an optics library.

Given the availability of these workarounds, and the greater simplicity and
predictability, we propose that ``OverloadedRecordUpdate`` will not permit
type-changing updates, as agreed in previous proposals.



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
  type SetFieldPoly :: forall {l} . Symbol -> Type -> Type -> TYPE l -> Constraint
  class SetFieldPoly x s t (b :: TYPE l) | x t -> b s l, x s -> t l where
    -- | Update function to set the field @x@ in the record @s@.  Permits
    -- type-changing update.
    setFieldPoly :: b -> s -> t

  -- | Constraint representing the fact that a field @x@ of type @a@ can be
  -- selected from the record type @r@.
  type SetField :: forall {l} . Symbol -> Type -> TYPE l -> Constraint
  type SetField x r a = SetFieldPoly x r r a

  -- | Update function to set the field @x@ in the record @r@.  Does not permit
  -- type-changing update.
  setField :: forall {l} x r (a :: TYPE l)  . SetField x r a => a -> r -> r
  setField = setFieldPoly @x

  -- | Constraint representing the fact that a field @x@ of type @a@ can be
  --  selected from or updated in the record @r@.
  type HasField :: forall {l} . Symbol -> Type -> TYPE l -> Constraint
  type HasField x r a = (GetField x r a, SetField x r a)

  -- | Constraint representing the fact that a field @x@ of type @a@ can be
  -- selected from the record @s@, or updated with a value of type @b@ to
  -- produce a record of type @t@.
  type HasFieldPoly :: forall {l} . Symbol -> Type -> Type -> TYPE l -> TYPE l -> Constraint
  type HasFieldPoly x s t a b = (GetField x s a, GetField x t b, SetFieldPoly x s t b)

  -- | If there is a field @x@ in the record type @r@, returns the type of the
  -- field.  The field must have a simple type of kind 'Type' (i.e. it may not
  -- be higher-rank, existential or unboxed).
  type family FieldType (x :: Symbol) (r :: Type) :: Type

To summarise the changes relative to the previously-accepted `proposal #158
<https://github.com/ghc-proposals/ghc-proposals/pull/158>`_:

* The ``HasField`` class has been renamed to ``GetField``.  In its place there
  is a new ``HasField`` constraint synonym for the pair of constraints
  ``GetField`` and ``SetField``.

* ``SetField`` is now a constraint synonym for ``SetFieldPoly``, a new class
  that permits type-changing update.  A new ``HasFieldPoly`` constraint synonym
  permits both field selection and type-changing update.

* The ``setField`` function now takes the field value first, followed by the
  record value.

* The classes are polymorphic in the runtime representation of the field type,
  allowing support for `Unlifted fields`_. Standalone kind signatures and
  explicit specificity annotations are used to make this polymorphism explicit.

* The classes are no longer polymorphic in the kind of field labels. This is now
  restricted to be ``Symbol``.

* A new ``FieldType`` type family makes it possible to look up the type of a
  field.

TODO: merge with other list

TODO: say somewhere that ``OverloadedRecordUpdate`` will change the order of the
arguments in calls to ``setField``, including with ``RebindableSyntax``.


Solving ``GetField`` constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The following is a specification of constraint solving behaviour for
``GetField``.  This is essentially unchanged from the solving behaviour for
``HasField`` in existing GHC versions, which is described in the `GHC user's
guide <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/hasfield.html#solving-hasfield-constraints>`_,
except that it should yield better error messages when a field is used at the
wrong type.

A wanted constraint ``GetField f r a`` will be resolved automatically by GHC's
constraint solver when the following hold:

* ``f`` is a type-level symbol ``"foo"``.

* ``r`` is an application of a record type ``R`` to some arguments ``t0 ... tn``.

* The record type ``R x0 ... xn`` has a field ``foo`` (of some type ``u[x0,...,xn]``).

* The field ``foo`` is in scope, according to the usual module scope rules.

* The actual field type ``u[x0,...,xn]`` does not refer to any
  existentially-quantified type variables or contain any universal quantifiers.

If the wanted field type ``a`` is apart from the actual field type
``u[x0,...,xn]``, the constraint solver will reject the constraint as insoluble
(with an appropriate error message).  Otherwise, the constraint solver will
discharge the original constraint, and emit new constraints:

* ``a ~ u[t0/x0,...,tn/xn]`` (equating the type from the wanted with the actual
  type of the field);

* TODO: something about GADTs;

* any constraints from the datatype context (defined with ``DatatypeContexts``),
  if there is one.

If the field is partial, and the new ``-Wincomplete-record-selectors`` flag is
enabled, a warning will be emitted.

Note that:

* If ``R`` is a data family, it is considered a record type iff there is an
  instance of the family for ``R t0 ... tn`` that is defined as a record.

* Solving the equation between the wanted and actual field types will fill in
  the inferred parameter ``l :: RuntimeRep`` with the appropriate
  representation.  This means support for unlifted fields is automatic.

TODO: explain when manual GetField instances are permitted.

TODO: discuss improving error messages in the set-with-wrong-type case!


Solving ``SetFieldPoly`` constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general, the constraint solving behaviour for ``SetFieldPoly`` is slightly
more complex than ``GetField``, because of the possibility of type-changing
updates.  When the original and updated record types are the same (e.g. the
``SetField`` constraint synonym is used), then the following rules specialise to
the rules for ``GetField``.  That is, a constraint ``SetFieldPoly f r r a`` will
be solved automatically iff ``GetField f r a`` is solved automatically.
(TODO: verify this claim.)

TODO: update the following

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

* at least one of the occurrences is rigid (i.e. not under a type family); (TODO: define more precisely)

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

If the field is partial, and the ``-Wincomplete-record-updates`` flag is
enabled, a warning will be emitted.

TODO: explain when manual SetFieldPoly instances are permitted?


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
that makes infrequent use of mechanisms that depend upon ``setField``, it is not
desirable to pay this cost for up front compilation of updaters.  Instead, by
default GHC should solve ``SetField`` constraints by generating an updater
function on-the-fly.

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

Record dot syntax
~~~~~~~~~~~~~~~~~
This proposal will change inferred types of expressions written with
``OverloadedRecordDot``, as we now have ``(.foo) :: GetField "foo" r a => r -> a``
instead of ``(.foo) :: HasField "foo" r a => r -> a``.  However, the existence
of the ``HasField`` constraint synonym should mean that user-written type
signatures mentioning ``HasField`` continue to be accepted.


OverloadedLabels
~~~~~~~~~~~~~~~~
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


Virtual fields
~~~~~~~~~~~~~~
A "virtual field" is an instance of a ``GetField`` or ``SetField`` constraint
that is defined explicitly by the user, and which does not correspond to an
existing record datatype.  For example::

  data V = MkV Int

  instance GetField "foo" V Int where
    getField (MkV i) = i

  instance SetFieldPoly "foo" V V Int Int where
    setField i (MkV _) = MkV i

Even though ``V`` is not defined as a record, the presence of these instances
means ``foo`` can be used as a field, e.g. ``let e = MkV i in e.foo`` is
accepted with ``OverloadedRecordDot``.  This can be particularly useful in
conjunction with record pattern synonyms, as pattern synonyms do not lead to
``GetField`` and ``SetField`` constraints being solved automatically (see
discussion of `Pattern synonyms`_ below).

Splitting ``HasField`` into separate ``GetField`` and ``SetField`` classes means
it is possible to define get-only or set-only virtual fields.


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

A workaround for this exists in the form of `Virtual fields`_ given by manual
``HasField`` instances.  For this example, the user could define an (orphan)
instance::

  instance a ~ b => GetField "theValue" (Maybe a) b where
    getField = theValue

For now we do not propose generating such instances automatically.  In
particular, this is complicated by the possibilities that pattern synonyms may
be defined independently of the underlying type (which would give rise to orphan
instances, as in the ``Maybe`` example), the type need not even be a record, and
multiple pattern synonyms may define conflicting fields for the same type.

TODO: perhaps we should revisit this, and only report errors if we actually hit
ambiguity when solving?


Modifiable parameters and multiple updates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A traditional ``Haskell2010`` record update may change multiple fields
simultaneously, which may be important when types change.  For example::

  data Pair a = MkPair { first :: a, second :: a }

  multipleUpdate :: Pair Int -> Pair String
  multipleUpdate x = x { first = show (first x + second x), second = "" }

Here it is crucial that both fields are changed simultaneously, because ``Pair``
requires both its components to have the same types.

In contrast, a call to ``setFieldPoly`` may change the value of only a single
field.  Consequently, if a datatype parameter occurs in the types of multiple
fields, it may not be changed via type-changing update.  We do not currently
have a good way to support such updates without introducing significant
complexity.

This is not a drastic limitation because it is usually possible to generalise
the record type involved so that each field has an independent type, for example
by defining::

  type Pair a = Pair' a a
  data Pair' a b = MkPair { first :: a, second :: b }

Now the following alternate definition is accepted, including a subexpression
whose type is ``Pair' String Int``::

  multipleUpdate :: Pair Int -> Pair String
  multipleUpdate x = (x { first = show (first x + second x) }) { second = "" }


Type parameters occurring under type families
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Consider the following definitions::

  data UnderFamily c = MkUnderFamily { foo :: F c }

  type family F (x :: Type) :: Type
  type instance F Int  = Int
  type instance F Bool = Bool
  type instance F Char = Bool

  underFamilyRecord :: UnderFamily Int
  underFamilyRecord = MkUnderFamily { foo = 0 }

In an update such as ``underFamilyRecord { foo = True }`` the resulting record
could have type ``UnderFamily Bool`` or ``UnderFamily Char`` because both would
be type-correct.  However, this means that the field name, initial record type
and assigned field type do not determine the resulting record type, i.e. the
functional dependency ``x s b -> t`` in the definition of ``SetFieldPoly`` would
be violated if the constraints
``SetFieldPoly (UnderFamily Int) (UnderFamily Bool) Int Bool`` and
``SetFieldPoly (UnderFamily Int) (UnderFamily Char) Int Bool`` were both
solvable.  As with the case of phantom parameters discussed above, this means
inferred types are not necessarily principal.

Thus we propose that the constraint solver should not allow ``SetFieldPoly``
constraints to change type parameters where the type variable appears only
"flexibly", i.e. under a type family application in the field type.

If a parameter occurs both "rigidly" and "flexibly", it is safe to allow
type-changing updates in involving that parameter.  For example::

  data Rigid c = MkRigid { bar :: (c, F c) }

  rigid :: Rigid Int
  rigid = (0, 0)

  ok = rigid { bar = (True, False) }

Here the only possible type of ``ok`` is ``Rigid Bool``, because it is
determined by the first component of the pair; the presence of the type family
doesn't make a difference.

TODO: rewrite this section as more of an example



Costs and Drawbacks
-------------------
This will require moderate development effort, as the current implementation of
``HasField`` constraint solving relies on generating selector/updater functions
up front, rather than constructing them during constraint solving as required by
``-fno-generate-record-updaters``.  It does not seem like it will introduce a
substantial maintenance burden.

Novice users may find ``HasField`` and overloaded record dot syntax more complex
to reason about than traditional Haskell record syntax.  However this proposal
has taken care to ensure the more complex aspects (e.g. type-changing update)
need not be exposed to those who do not go looking for them.

For users who do not wish to use ``HasField`` at all, the approach taken in this
proposal should mean they do not pay a compile-time performance cost, and can
happily ignore the ``GHC.Records`` module and record dot syntax extensions.


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

* @effectfully described the `SameModulo approach
  <https://github.com/effectfully-ou/sketches/tree/master/has-lens-done-right#the-samemodulo-approach-full-code>`_
  which uses type families and an additional class to give a clever encoding of
  type-changing update that supports phantom parameters and occurrences of type
  variables under type families.

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

Subsequent subsections discuss alternative choices for particular aspects of the
design recommended by this proposal.


Order of arguments to setField
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`Proposal #158 <https://github.com/ghc-proposals/ghc-proposals/pull/158>`_
specifies that the type of ``setField`` is ``HasField x r a => r -> a -> r``.
However, swapping the order of arguments so that the new field value is first
means that composing of multiple updates for a single record becomes simpler::

  setField :: HasField x r a => a -> r -> r

  example :: (HasField "age" r Int, HasField "colour" r String) => r -> r
  example = setField @"age" 42 . setField @"colour" "Blue"

While we do not typically expect users to call ``setField`` directly, in cases
where they prefer to do so, this seems like a good reason to prefer this
argument order.  Moreover, this order is consistent with the ``set`` function in
the ``lens`` and ``optics`` libraries.  It is not clear what the rationale was
for the alternative order in the previous proposal.

This proposal assumes that calls to ``setField`` take the field value first,
followed by the record.


Single class vs. multiple classes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The dormant `proposal #286
<https://github.com/ghc-proposals/ghc-proposals/pull/286>`_ suggests splitting
``HasField`` into two classes, ``GetField`` and ``SetField``, permitting
selection and update respectively.  The previous proposal was primarily
motivated by the possibility of supporting read-only (virtual) fields.  (There
is no proposed mechanism for normal record fields to be marked as being
read-only or write-only to limit when the constraints should be solved
automatically, but in principle this would be possible.)

We also propose splitting ``HasField`` into separate classes for selection and
update, for the following additional reasons:

* It allows more precise types: a function of type
  ``(GetField "foo" r Int, SetField "bar" r Bool) => r -> r`` obviously can only
  read the ``foo`` field and write the ``bar`` field.

* It allows `Warnings for partial fields`_ that accurately reflect whether the
  field is being selected or updated.

* It should lead to better compile-time performance (see `Compilation time
  benefits of splitting classes`_).


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
possibility of set-only fields.

Instead we propose that ``GetField`` and ``SetField`` should be independent
classes, with no superclasses, and that ``HasField`` should be a constraint
synonym for both constraints.  That is, ignoring type-changing update and
questions around functional dependencies vs. type families for now, the design
would look something like::

  class GetField x r a where
    getField :: r -> a

  class SetField x r a where
    setField :: a -> r -> r

  type HasField x r a = (GetField x r a, SetField x r a)

Since ``GetField`` and ``SetField`` are independent, the underlying dictionaries
are newtypes, which would not be the case if there were superclasses involved.

Including the ``HasField`` constraint synonym means that where both ``getField``
and ``setField`` are used, users can write simpler types, and GHC can use it to
represent inferred types more simply.

This change is not entirely backwards compatible.  Existing code using
``HasField`` should mostly continue to work, provided it does not define virtual
fields or use an explicit import such as ``import GHC.Records (HasField(getField))``.
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
``hasField :: r -> (a -> r, a)``.  This represents a lens, i.e. the combination
of a getter and setter into a single value, although it uses a first-order
representation that is simpler and does not compose as well as the "van
Laarhoven" representation of lenses.

However practical cases where the choice of ``hasField``
vs. ``getField``+``setField`` matters are likely to be rare.  In particular,
normal record types with the built-in constraint-solving behaviour do not gain
anything from ``hasField``. Where this matters, users are likely to be better
off using an optics library.  Thus we prefer the simplicity of separate classes
in the ``GHC.Records`` API.

If users do wish to organise field-like lenses into a class, they can define an
auxiliary class such as the following::

  class HasField x r a => HasFieldLens x r a where
    fieldLens :: Lens' x r a
    fieldLens = lens getField setField

  -- Instance will be selected by default, but can be overridden by defining an
  -- instance for a specific type with a non-default `fieldLens` implementation
  instance {-# OVERLAPPABLE #-} HasField x r a => HasFieldLens x r a

We do not propose to add such a class to ``GHC.Records``, since it is better
defined by specific optics libraries.  (The ``optics`` library defines a class
``LabelOptic`` that plays essentially this role.)


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
  because unsolved constraints look like ``HasField x r a`` which has a natural
  reading "``r`` has a field ``x`` of type ``a``".  In contrast, the type family
  approach ends up with unsolved ``HasField x r`` constraints (meaning ``r`` has
  a field ``x`` of unspecified type) and equalities including ``FieldType``.
  (See `previous discussion on proposal #158
  <https://github.com/ghc-proposals/ghc-proposals/pull/158#issuecomment-449419429>`_.)

* Supporting `Unlifted fields`_ with the type family approach would introduce
  extra complexity, because we would need another type family to determine the
  ``RuntimeRep`` of the field, and it would be difficult to hide this type
  family from users.  In contrast, supporting them is relatively straightforward
  with functional dependencies, and GHC will automatically hide unused levity
  polymorphism.

* For `type-changing update`_, it is desirable that either the original or
  updated types may be used to infer the other.  This can be achieved with type
  families (e.g. see `the SameModulo approach by @effectfully
  <https://github.com/effectfully-ou/sketches/tree/master/has-lens-done-right#the-samemodulo-approach-full-code>`_)
  but requires additional complexity.

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


Higher-rank fields
~~~~~~~~~~~~~~~~~~
Consider the following::

  data Rank1 = Rank1 { identity :: forall a . a -> a }

  data Rank2 = Rank2 { withIdentity :: (forall a . a -> a) -> Bool }

In the first definition, the field has a rank-1 type, but this means the
selector function has a type with a ``forall`` to the right of an arrow.
Similarly, in the second definition, a rank-2 field type leads to a higher-rank
selector function type::

  identity     :: Rank1 -> forall a . a -> a  -- NOT forall a . Rank1 -> a -> a (in recent GHCs)

  withIdentity :: Rank2 -> (forall a . a -> a) -> Bool

Should it be possible to solve ``GetField`` or ``SetFieldPoly`` constraints
involving such fields?  Unfortunately it is not feasible to solve for
"impredicative" constraints such as
``GetField "identity" Rank1 (forall a . a -> a)``,
even with the recent introduction of Quick Look Impredicativity (following
`proposal #274 <https://github.com/ghc-proposals/ghc-proposals/pull/274>`_).
Bidirectional type inference, on which both ``RankNTypes`` and
``ImpredicativeTypes`` (now) rely, requires that instantiations of
``forall``-bound variables be determined while traversing the term, prior to the
constraint solver being invoked.

On the other hand, it would be possible in principle to solve constraints such
as ``GetField "identity" Rank1 (a -> a)`` for arbitrary ``a``, making it appear
as if the field has an infinite family of types.  However, this does not extend
to ``SetField``, because there we really need the value being set to be
polymorphic.  Moreover, it violates the functional dependency ``x r -> a`` on
the ``GetField`` class.

Accordingly, we propose that ``GetField`` or ``SetFieldPoly`` constraints
involving fields with higher-rank types should not be solved automatically.
(This is the existing behaviour for ``HasField`` in current GHC versions.)


Partial fields
~~~~~~~~~~~~~~
In ``Haskell2010`` it is permitted to define *partial fields*, i.e. fields that
do not belong to every constructor of the datatype.  This means that traditional
record selection and update may throw runtime exceptions, as in these examples::

  data T = MkT1 { partial :: Int } | MkT2

  t = MkT2
  oops1 = partial t
  oops2 = t { partial = 0 }

Many Haskell programmers prefer not to define partial fields, as part of a
general desire to avoid unnecessary partiality (see for example `proposal #351
<https://github.com/ghc-proposals/ghc-proposals/pull/351>`_).

Partial fields may be identified at definition sites via the existing
``-Wpartial-fields`` warning.  However, this is somewhat conservative: it is
perfectly safe to *define* partial fields provided they are *used* only via
record construction and pattern-matching, not via selection or update.  Users
have `asked for the ability to prevent unsafe uses while permitting datatype
definitions
<https://www.reddit.com/r/haskell/comments/ln6eu1/implementation_of_nofieldselectors_is_merged/gnzviyt/>`_,
because giving field names can help with readability when a datatype has many
constructors and many fields.


Warnings for partial fields
^^^^^^^^^^^^^^^^^^^^^^^^^^^
There is an existing warning flag ``-Wincomplete-record-updates`` that will emit
a warning when a traditional record update refers to a partial field.  However,
there is no corresponding flag for traditional selector functions, though it has
been requested (`#7169 <https://gitlab.haskell.org/ghc/ghc/-/issues/7169>`_,
`#17100 <https://gitlab.haskell.org/ghc/ghc/-/issues/17100>`_).  (The
``NoFieldSelectors`` extension can be used to banish such selectors altogether.)

At present, the automatic solving of ``HasField`` constraints for partial fields
will silently make use of partial selector functions, without emitting a
warning.  So far no proposal has considered this issue in the context of
introducing ``setField`` (though see `#18650
<https://gitlab.haskell.org/ghc/ghc/-/issues/18650>`_).

To address this, we propose:

* adding a new flag ``-Wincomplete-record-selectors`` that will warn on
  occurrences of partial selector functions, including when they are used to
  solve ``GetField`` constraints;

* extending the existing ``-Wincomplete-record-updates`` to warn when a
  ``SetField`` constraint is solved for a partial field.

The new warnings would not (for now) be implied by ``-Wall``, just as
``-Wincomplete-record-updates`` and ``-Wpartial-fields`` are not.

This does not make it possible for a library author to define a datatype with
partial fields such that their users *cannot* use partial operations (even under
``NoFieldSelectors``, it will still be possible to solve ``GetField``
constraints and hence use record dot syntax for selection).  Instead, downstream
modules will need to enable
``-Werror=incomplete-record-selectors -Werror=incomplete-record-updates`` in
order to rule out such cases.  We could imagine somehow annotating datatypes to
impose restrictions such as preventing selection or update, but this is not part
of the current proposal.


Affine traversals
^^^^^^^^^^^^^^^^^
Optics libraries in principle have a better story to tell here. Partial fields
give rise to *affine traversals*, where the accessor function returns a
``Maybe`` value and the setter leaves the value unchanged if it does not mention
the field (rather than throwing a runtime exception).

We could consider supporting this using built-in classes like the following::

  class GetPartialField x r a | x r -> a where
    getPartialField :: r -> Maybe a

  class SetPartialField x s t a b | x s -> a, x t -> b, x s b -> t, x t a -> s where
    setPartialField :: b -> s -> t

  type family FieldTotal x (r :: Type) :: Bool

Note that ``setField`` and ``setPartialField`` have the same type, but
``setField`` throws an exception on missing fields, whereas ``setPartialField``
returns the value unchanged.

For now we propose not to include support for partial fields through the
``GetPartialField`` and ``SetPartialField`` constraints and ``FieldType`` type
family, although they might be considered again in the future.


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


Option: discouraging ``HasField`` abstraction, defaulting based on fields in scope
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general, users will get the best results if ``HasField`` constraints are used
as a mechanism for the compiler to resolve potentially-ambiguous field names
internally within definitions, but not abstracted over to produce overloaded
definitions.  This is not just about type inference, but about good design:
having an interface that depends on ``HasField`` exposes too much about the
implementation (namely the fields it accesses).  Moreover, field names in
``HasField`` constraints are bare strings that do not carry any meaning.  Thus
where abstracting over fields is necessary in an interface, users should be
encouraged to do so explicitly (e.g. passing projection functions as arguments,
or introducing a custom typeclass) rather than using ``HasField``.

One option for discouraging excessive abstraction would be for GHC to refuse to
generalise over inferred ``HasField`` constraints.  That is, unless the user
explicitly wrote a type signature indicating that a definition should be
polymorphic in ``HasField``, GHC would require the constraints to determine a
record type and return an error if they did not.

This could be combined with a defaulting step based on the fields currently in
scope.  If there is an unsolved ``HasField "foo" s a`` constraint, and there is
exactly one ``foo`` field in scope, the constraint solver could default ``s`` to
be the type containing that field.  This would mean that code using traditional
Haskell record updates would be less likely to be generalised (perhaps
introducing ambiguity) when the ``OverloadedRecordUpdate`` extension was
enabled, so it would make ``OverloadedRecordUpdate`` more backwards-compatible.
However, it might surprise users that bringing a second ``foo`` field into scope
would suddenly lead to unsolved constraints.


Option: defaulting type-changing updates to be monomorphic
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TODO: discuss option to default type-changing updates to be monomorphic


Phantom parameters
~~~~~~~~~~~~~~~~~~
TODO: amend the following to refer back to the earlier discussion,
and raise the question of whether we should refuse to solve phantom updates anyway.

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



Unresolved Questions
--------------------

* Should ``OverloadedRecordUpdate`` permit type-changing update via ``SetFieldPoly``?

* Is the proposed constraint-solving behaviour for ``SetFieldPoly``
  satisfactory?

* ``SetFieldPoly`` is a terrible name. What should it be called?

* Does the ``FieldType`` type family pull its weight?  It is not necessary for
  normal use of ``HasField``, and can be approximated using ``GHC.Generics``.

* Are there other design choices surrounding ``HasField`` not considered here?


Implementation Plan
-------------------
The proposal author, Adam Gundry, will implement this change if accepted.  The
implementation of this proposal (or some other way to support ``setField``) is
currently blocking the full implementation of ``OverloadedRecordUpdate``
(`proposal #282 <https://github.com/ghc-proposals/ghc-proposals/pull/282>`_).


Endorsements
-------------
(Optional) This section provides an opportunty for any third parties to express their
support for the proposal, and to say why they would like to see it adopted.
It is not mandatory for have any endorsements at all, but the more substantial
the proposal is, the more desirable it is to offer evidence that there is
significant demand from the community.  This section is one way to provide
such evidence.
