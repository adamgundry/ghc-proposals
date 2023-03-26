Type-changing update for ``SetField``
=====================================

.. author:: Adam Gundry
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

This proposal builds on the `HasField redesign proposal
<https://github.com/adamgundry/ghc-proposals/blob/hasfield-redesign/proposals/0000-hasfield-redesign.rst>`_,
extending it with type-changing update.  It assumes that the approach proposed
there is adopted, in particular having a new ``SetField`` typeclass for updates
that is separate from the existing ``HasField`` typeclass.  For background on
overloaded record fields and ``SetField``, please refer to that proposal.

TODO: This needs reworking once the above proposal stabilises, to make sure it
uses the final design.  In particular, it is not yet completely consistent
whether the proposed ``SetField`` class has parameters ``x s t b`` (defining
``setField``) or ``x s t a b`` (defining ``modifyField``).


Motivation
----------

A traditional ``Haskell2010`` record update such as ``t { foo = e }`` is able to
change the type of the field being updated, and hence the type of the record as
a whole.  For example::

  data T a = MkT { foo :: a }

  typeChangingUpdate :: T () -> T Bool
  typeChangingUpdate t = t { foo = True }

Type inference for such definitions is relatively unproblematic in traditional
Haskell, because the field name must uniquely determine the record type being
updated, or else the definition is rejected as ambiguous.

The ``SetField`` class will provide a mechanism for updating fields where the
record type is determined by type inference, like this::

  class SetField x r a | x r -> a where
    modifyField :: (a -> a) -> r -> r

  setField :: SetField x r a => a -> r -> r
  setField = modifyField . const

This is intended for use either with the ``OverloadedRecordUpdate`` extension,
which provides syntactic sugar for calls to ``setField``, or with a lens/optics
library.  For example, the ``optics`` package can automatically use
``OverloadedLabels`` as lenses using ``getField`` and ``modifyField`` (with the
latter currently implemented using generic programming, in the absence of
built-in support).

However, this design does not permit type-changing updates, because it defines a
setter operation ``modifyField :: SetField x r a => (a -> a) -> r -> r`` where the input
and output record types must both be ``r``.  This has the significant merit of
simplicity, because type inference has more information to work with, and there
is no need to specify under which circumstances type-changing updates are
allowed.

However, type-changing updates are desirable for libraries such as ``optics``.
Moreover, some people would prefer type-changing update to be supported by
``OverloadedRecordUpdate``, although this is controversial.

In the light of this, we propose adding support for type-changing update to the
``GHC.Records`` API.  In particular, ``GHC.Records`` will expose both a function
``setField`` that permits type-changing update and a function ``setField'``
that specialises it to the case when type-changing update is not available::

  class SetField x s t a b | ... where
    modifyField :: (a -> b) -> s -> t

  type SetField' x r a = SetField x r r a a

  setField :: forall x s t a b . SetField x s t a b => b -> s -> t
  setField = modifyField . const

  setField' :: forall x r a . SetField' x r a => a -> r -> r
  setField' = setField @x

Crucially, using the ``SetField'`` constraint synonym or the ``setField'``
function ensures that the record type cannot change, so type inference behaviour
should be exactly the same as if type-changing update were not available at all.
However, users who need type-changing update can use ``SetField`` instead.

This leaves open two questions:

* How should type inference work for ``SetField`` constraints?

* Should record update syntax permit type-changing update?


The challenge of type inference for ``SetField`` constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TODO: the following needs updating with the extra ``a`` parameter to ``SetField``!

For a ``HasField`` constraint, the constraint solver will automatically solve a
constraint like ``HasField "f" (T a b c) ty`` when ``T`` is a record datatype
with a field ``f`` in scope.  That is, given a constraint ``HasField x r a``
the ``x`` parameter must be a ``Symbol`` literal, the ``r`` parameter must be
a record type constructor (applied to some arguments), and the record must have
a field of the appropriate name.

Correspondingly, we expect a non-type changing ``SetField' "f" (T a b c) ty``
constraint, which is equivalent to ``SetField "f" (T a b c) (T a b c) ty``,
to be solved automatically in the same way.

However, this is not enough if we want to allow type-changing update.  For example::

  data T a = MkT { f :: a }

  fun1 :: T () -> T Int
  fun1 t = setField @"f" 0 t
  -- constraints arising:  SetField "f" (T ()) (T Int) alpha  (Num alpha)

Here the ``SetField`` constraint arises from the call to ``setField``,
and ``alpha`` is a unification variable representing the type of the numeric
literal ``0``.  The ``SetField`` constraint is easily solved as we do not
require the type parameters for the two occurrences of ``T`` to be the same, and
we do not need the field type to be determined.  Instead, we can see that the
record type being updated is ``T``, and infer that the field type ``alpha`` from
the constraint must unify with the actual type of the ``f`` field of ``T Int``,
namely ``Int``.

More interesting cases arise if we have partial type information::

  fun2 t = setField @"f" 0 (t :: T ())
  -- interim inferred type:  T () -> beta
  -- constraints arising:  SetField "f" (T ()) beta alpha  (Num alpha)
  -- final inferred type:  Num a => T () -> T a

  fun3 t = (setField @"f" 0 t) :: T Int
  -- interim inferred type:  gamma -> T Int
  -- constraints arising:  SetField "f" gamma (T Int) alpha  (Num alpha)
  -- final inferred type:  T a -> T Int

In each case the comment shows the ``SetField`` constraint that arises.  We
can handle these constraints too, by exploiting the fact that type-changing
update does not change the choice of record type constructor, merely its
parameters.  Thus if *either* the ``s`` or ``t`` parameters is a concrete record
type, we can infer that the other parameter must be some instance of the same
record type/ For example, in the ``fun2`` case we infer that ``beta ~ T alpha1``
for some fresh unification variable ``alpha1``, then unify the types for the
field to get ``alpha ~ alpha1``.

On the other hand, if neither record parameter is a concrete record type, we
cannot determine the record type and solve the ``SetField`` constraint but
must generalise over it in the usual way::

  fun4 t = setField @"f" 0 t
  -- interim inferred type:  delta -> epsilon
  -- constraint arising:  SetField "f" delta epsilon alpha
  -- final inferred type:  (Num b, SetField "f" s t b) => s -> t

To recap, we have seen that it is unproblematic to support type-changing update
where the record type is concrete (either before or after the update), and that
simple cases of polymorphic updates are possible.

However, things become more difficult if we try to *compose* polymorphic
updates.  For example::

  fun5 t = setField @"g" True . setField @"f" () $ t
  -- interim inferred type: beta -> delta
  -- constraints arising:  SetField "f" beta gamma ()
  --                       SetField "g" gamma delta Bool
  -- final inferred type:  (SetField "f" s t (), SetField "g" t u Bool) => s -> u

Here we have an ambiguity problem: the type variable ``t`` is ambiguous, because
it appears only in the context to the left of the ``=>`` sign.  But rejecting
this definition would be distinctly unsatisfactory, because it is perfectly
possible to call ``fun5`` unambiguously: in a context that fixes ``s`` or ``u``
to be a concrete record type with ``f`` and ``g`` fields, the ``SetField``
constraints will become solvable, and will determine the middle type ``t``
automatically.


Functional dependencies to the rescue?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The usual solution to such ambiguity problems would be to introduce functional
dependencies between the parameters of the typeclass, e.g. previous designs for
type-changing update have used something like::

  class SetField x s t a b | x s -> a, x t -> b, x s b -> t, x t a -> s where
    modifyField :: (a -> b) -> s -> t

Here the functional dependency ``x s b -> t`` asserts that the field name ``x``,
input record type ``s`` and new field type ``b`` can be used to determine the
output record type ``t``.  This would mean ``fun5`` was accepted without
ambiguity, because the functional dependency can be used to determine ``t`` from
``"f"``, ``s`` and ``()`` in ``SetField "f" s t a ()``.

TODO: the following needs updating with the extra ``a`` parameter to ``SetField``!

Unfortunately, this functional dependency is not sufficient to handle the
following example, where the field types are not uniquely determined, so ``t``,
``a`` and ``b`` are all ambiguous::

  fun6 t = setField @"k" 0 . setField @"h" [] $ t
  -- interim inferred type: beta -> delta
  -- constraints arising:  SetField "h" beta gamma [alpha]
  --                       SetField "k" gamma delta epsilon  (Num epsilon)
  -- final inferred type:  (Num b, SetField "h" s t [a], SetField "k" t u b) => s -> u

Nor can it handle examples where inference needs to proceed "in reverse" from
the result type of the update to the type being updated, e.g. here ``s`` is
ambiguous::

  fun7 () = setField @"l" () undefined
  -- interim inferred type: () -> gamma
  -- constraints arising: SetField "l" beta gamma ()
  -- final inferred type:  SetField "l" s t () => () -> t

Not only does the functional dependency ``x s b -> t`` fail to determine enough
type variables unambiguously, but also it is too restrictive, because it rules
out certain type-changing updates that are accepted by traditional Haskell
record updates.  For example, this arises with phantom type parameters::

  data Tagged u w = Tagged { unTagged :: w }

  -- with traditional Haskell records:
  phantomTypeChangingUpdate1 x = x { unTagged = unTagged x }
  -- inferred type: Tagged u w -> Tagged v w

  -- with overloaded update:
  phantomTypeChangingUpdate2 x = setField @"unTagged" (unTagged x) x
  -- interim inferred type: Tagged u beta -> gamma
  -- constraints arising:  SetField "unTagged" (Tagged u beta) gamma beta
  -- final inferred type: SetField "unTagged" (Tagged u w) (Tagged v w) w => Tagged u w -> Tagged v w

Here we have a constraint where the record type is known, but solving the
constraint would violate the ``x s b -> t`` functional dependency, because ``t =
Tagged v w`` has an occurrence of ``v`` that is not determined by ``x =
"unTagged"``, ``s = Tagged u w``, ``b = w``.


The solution: non-functional dependencies
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
We have seen that the ``x s b -> t`` functional dependency is both insufficient
for good type inference, and yet rules out some type-changing updates.  How
might we do better?

Consider the dependencies ``x s -> t, x t -> s b``. At first glance, these are
somewhat surprising: they claim that if we know the field name ``x``, then
knowledge of either ``s`` or ``t`` will allow the field type and the other
instantiation of the record type to be determined.  This is clearly not true if
type-changing updates are permitted.  For example, GHC would not normally allow
us to define::

  instance SetField "unTagged" (Tagged s a) (Tagged t b) a b

because it would violate the liberal coverage condition.

The trick is to interpret these arrows not as functional dependencies, which
require the targets to be fully determined from the sources, but rather as
assertions that type inference will be able to (partially) improve the targets
if (partial) information about the sources is available.  Thus the coverage
condition need not be imposed for these dependencies.

Now examples like ``fun6`` and ``fun7`` are no problem, because they are not
considered ambiguous.


Proposed Change Specification
-----------------------------

Changes to ``GHC.Records``
~~~~~~~~~~~~~~~~~~~~~~~~~~

The definitions of ``SetField`` and ``Field`` in ``GHC.Records`` (according to
`proposal #583 <https://github.com/ghc-proposals/ghc-proposals/pull/583>`_) are
replaced with the following (``HasField`` is unchanged)::

  -- | Constraint representing the fact that a field @x@ of type @a@ can be
  -- updated in the record type @s@, producing a record of type @t@.
  --
  -- This will be solved automatically for built-in records where the field is
  -- in scope, but manual instances may be provided as well.
  --
  -- Where a 'HasField' instance is available alongside an instance of this
  -- class, they must satisfy the laws defined on 'Field'.
  --
  type SetField :: forall {k} {s_rep} {t_rep} {a_rep} {b_rep} . k -> TYPE s_rep -> TYPE t_rep -> TYPE a_rep -> TYPE b_rep -> Constraint
  class SetField x s t a b | x s -> a, x t -> b where
    -- | Update function to set the field @x@ in the record @s@.  Permits
    -- type-changing update.
    modifyField :: (a -> b) -> s -> t

  -- | Constraint representing the fact that a field @x@ of type @a@ can be
  -- selected from the record type @r@.
  type SetField' :: forall {k} {r_rep} {a_rep} . k -> TYPE r_rep -> TYPE a_rep -> Constraint
  type SetField' x r a = SetField x r r a a

  setField :: forall {k} {s_rep} {t_rep} {a_rep} {b_rep} (x :: k) (s :: TYPE s_rep) (t :: TYPE t_rep) (a :: TYPE a_rep) (b :: TYPE b_rep) . SetField x s t a b => b -> s -> t
  setField = modifyField . const

  -- | Update function to set the field @x@ in the record @r@.  Does not permit
  -- type-changing update.
  setField' :: forall {k} {r_rep} {a_rep} (x :: k) (r :: TYPE r_rep) (a :: TYPE a_rep) . SetField' x r a => a -> r -> r
  setField' = setField @x

  -- | Constraint representing the fact that a field @x@ of type @a@ can be
  --  selected from or updated in the record @r@.
  type Field' :: forall {k} {r_rep} {a_rep} . k -> TYPE r_rep -> TYPE a_rep -> Constraint
  type Field' x r a = (HasField x r a, SetField' x r a)

  -- | Constraint representing the fact that a field @x@ of type @a@ can be
  -- selected from the record @s@, or updated with a value of type @b@ to
  -- produce a record of type @t@.
  --
  -- Where both 'HasField' and 'SetField' instances are defined for the
  -- same type, they must satisfy the following laws:
  --
  -- > getField @x (setField @x v r) === v
  -- > setField @x (getField @x r) r === r
  --
  type Field :: forall {k} {s_rep} {t_rep} {a_rep} {b_rep} . k -> TYPE s_rep -> TYPE t_rep -> TYPE a_rep -> TYPE b_rep -> Constraint
  type Field x s t a b = (HasField x s a, HasField x t b, SetField x s t a b)



Solving ``SetField`` constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A wanted constraint ``SetField x s t a b`` will be resolved automatically by
GHC's constraint solver when the following hold:

* ``x`` is a type-level symbol ``"foo"``.

* At least one of ``s`` or ``t`` is an application of a record type ``R``.

* The record type ``R`` has a field ``foo``, and this field is in scope
  according to the usual module scope rules (qualified or unqualified).

* The updater function ``\ (f :: a -> b) (r :: s) -> r { foo = f (foo r) } :: t`` is
  well-typed modulo some new constraints *Cs*.

* There are no user-defined ``SetField`` instances that overlap with ``SetField
  "foo" (R ...) (R ...) a b``.

In the updater function , ``foo`` is taken to unambiguously reference the field
of ``R``, regardless of what else may be in scope. That is, the use of ``r { foo
= f (foo v) }`` in the updater function should be interpreted as syntactic sugar for a
case expression, e.g. if ``R`` has a single constructor ``MkR``, it will desugar
to ``case r of MkR{foo=x, ..} -> MkR{foo = f x, ..}``.

Any new constraints *Cs* required for the updater function to be well-typed will
be emitted by the constraint solver for subsequent solving.  The updater
function itself provides the dictionary corresponding to the ``SetField``
constraint.

In general, the constraint solving behaviour for ``SetField`` is slightly more
complex than ``HasField``, because of the possibility of type-changing updates.
However, when the original and updated record types are the same (e.g. the
``SetField'`` constraint synonym is used), a constraint ``SetField x r r a a``
will be solved automatically iff ``HasField x r a`` is solved automatically.

TODO: need to verify the above assertion.

Assuming `proposal #515
<https://github.com/ghc-proposals/ghc-proposals/pull/515>`_ is accepted,
user-defined ``SetField`` instances may overlap with the automatic behaviour,
and this will be reported as an overlapping instance error.  (If this proposal
is not accepted, ``SetField`` should be subject to restrictions on the
definition of user-defined instances that prevent such overlap, to be consistent
with ``HasField``.)


Type improvement for ``SetField``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The only true functional dependencies on ``SetField`` are ``x s -> a``, ``x t -> b``, i.e. the
(source or target) record type determines the field type it contains.

However, in certain cases ``SetField`` is treated as if it additionally had
functional dependencies ``x s -> t`` and ``x t -> s``.  Specifically, these
dependencies are treated as if they existed for:

* The ambiguity check: for example, a type ``SetField "foo" s t a b => t`` is
  not considered ambiguous.

* The instance consistency check: for example, the user may not simultaneously
  define instances ``SetField "foo" T T Int Int`` and ``SetField "foo" T T Char Char``.

* Improvements arising from instance declarations: for example, if there is a
  user-defined instance ``SetField "foo" T T Int Int`` then a wanted constraint
  ``SetField "foo" T alpha beta gamma`` will be solved using the dependency ``x s ->
  t`` to improve ``alpha := T`` followed by using the dependency ``x s -> a``
  to improve ``beta := Int`` and ``x t -> b``
  to improve ``gamma := Int`` at which point the instance applies.

However, these dependencies are ignored for the purposes of:

* Wanted-wanted constraint interactions: for example, given wanted constraints
  ``SetField x s t a b`` and ``SetField x s t' a b`` GHC will not infer that ``t ~
  t'``.

* The coverage condition: for example, the user may define an instance
  ``SetField "foo" (T a) (T b) Int Int`` (e.g. if ``T`` has a phantom parameter).



Examples
--------
TODO:
This section illustrates the specification through the use of examples of the
language change proposed. It is best to exemplify each point made in the
specification, though perhaps one example can cover several points. Contrived
examples are OK here. If the Motivation section describes something that is
hard to do without this proposal, this is a good place to show how easy that
thing is to do with the proposal.


Type parameters occurring under type families
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
functional dependency ``x s b -> t`` in the definition of ``SetField`` would
be violated if the constraints
``SetField (UnderFamily Int) (UnderFamily Bool) Int Bool`` and
``SetField (UnderFamily Int) (UnderFamily Char) Int Bool`` were both
solvable.  As with the case of phantom parameters discussed above, this means
inferred types are not necessarily principal.

Thus we propose that the constraint solver should not allow ``SetField``
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



Effect and Interactions
-----------------------
TODO:
Your proposed change addresses the issues raised in the motivation. Explain how.

Also, discuss possibly contentious interactions with existing language or compiler
features. Complete this section with potential interactions raised
during the PR discussion.


Costs and Drawbacks
-------------------
TODO:
Give an estimate on development and maintenance costs. List how this effects
learnability of the language for novice users. Define and list any remaining
drawbacks that cannot be resolved.


Alternatives
------------


* @effectfully described the `SameModulo approach
  <https://github.com/effectfully-ou/sketches/tree/master/has-lens-done-right#the-samemodulo-approach-full-code>`_
  which uses type families and an additional class to give a clever encoding of
  type-changing update that supports phantom parameters and occurrences of type
  variables under type families.


We propose to use ``SetField`` as the name for the class that supports
type-changing update, and ``SetField'`` for the non-type-changing
specialisation.  This use of the prime (``'``) is consistent with the ``lens``
and ``optics`` libraries.


"Dysfunctional" dependencies
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The proposed special treatment of the dependencies on the ``SetField`` class is
related to "dysfunctional instances" for selectively lifting the coverage
condition as `in proposal #374
<https://github.com/ghc-proposals/ghc-proposals/pull/374>`_.  However, lifting
the coverage condition can lead to non-confluence of constraint solving and
violation of the principal types property.

Dysfunctional dependencies lead to non-confluence
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
For example, suppose ``SetField`` had functional dependencies ``x s -> t, x t ->
s`` but "dysfunctional instances" were allowed to violate the coverage
condition. Thus the following definitions would be permitted::

  data Tagged u w = Tagged { unTagged :: w }
  instance {-# DYSFUNCTIONAL #-} SetField "unTagged" (Tagged u a) (Tagged v b) a b

Now consider the following set of wanted constraints::

  beta  ~ Tagged Int  ()
  gamma ~ Tagged Char ()
  SetField "unTagged" alpha beta  () ()
  SetField "unTagged" alpha gamma () ()

The constraint solving strategy GHC uses is to simplify equality constraints
first, giving::

  SetField "unTagged" alpha (Tagged Int  ()) () ()
  SetField "unTagged" alpha (Tagged Char ()) () ()

These can then be solved by improving ``alpha := Tagged delta epsilon`` using
the functional dependency ``x t -> s``.

However, starting from the same original set of constraints, if the constraint
solver began by using the wanted-wanted interaction with the functional
dependency ``x s -> t`` to generate the improvement ``beta ~ gamma``, it would
then hit the unsolvable constraint ``Int ~ Char``.

This proposal avoids this problem because it does not perform the wanted-wanted
interaction step.  Thus only the first sequence of constraint simplifications is
valid, and constraint solving should remain confluent.

Dysfunctional dependencies break principal types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
TODO: update for extra parameter for ``SetField``

For example: ::

  hmm v r = (setField @"foo" v r, setField @"foo" v r)
  -- interim inferred type: alpha -> beta -> (gamma, delta)
  -- constraints arising:  SetField "foo" beta gamma alpha
  --                       SetField "foo" beta delta alpha
  -- final inferred type:  SetField "foo" s t b => b -> s -> (t, t)
  -- most general type:    (SetField "foo" s t b, SetField "foo" s t' b) => b -> s -> (t, t')

Here the two wanted constraints lead to a functional dependency improvement
``gamma ~ delta``, so the inferred type has a single ``SetField``
constraint.  However, the most general (principal) type has two ``SetField``
constraints. According to the usual reading of the functional dependency, the
most general type is equivalent to the inferred type.  However, if
"dysfunctional" solutions are allowed, the two types are distinguishable.



Option: defaulting type-changing updates to be monomorphic
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TODO: discuss option to default type-changing updates to be monomorphic?


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
``setField`` should be able to change a phantom parameter, i.e.  whether a
constraint such as ``SetField "unTagged" (Tagged s1 a) (Tagged s2 b) a b``
should be solvable.

Moreover, in some use cases for phantom parameters, it is intended that only
trusted code modifies the parameter.  This is typically enforced at module
boundaries by hiding the data constructor, but as the example above
demonstrates, it is also necessary to hide any fields.  This seems undesirable,
as it may not be obvious to users that merely exporting a field allows any
phantom parameters to be changed arbitrarily.

Thus we propose that the constraint solver should not allow ``SetField``
constraints to change phantom parameters.  In cases where this is necessary, the
user can write a function that pattern matches on the data constructor (provided
it is in scope!).



Should ``OverloadedRecordUpdate`` use type-changing update?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The original plan for the ``OverloadedRecordUpdate`` extension (`proposal #282
<https://github.com/ghc-proposals/ghc-proposals/pull/282>`_ and `proposal #405
<https://github.com/ghc-proposals/ghc-proposals/pull/405>`_) was that it would
**not** permit type-changing updates.  Thus, turning on
``OverloadedRecordUpdate`` would cause the definition of ``typeChangingUpdate``
above to be rejected, which is unfortunate.

Regardless of type-changing update, there are various cases that may be present
in existing code but ``OverloadedRecordUpdate`` cannot support, such as those
involving higher-rank fields, or where multiple fields must be updated
simultaneously.

TODO: the following needs rewriting in the light of https://github.com/adamgundry/ghc-proposals/blob/disambiguating-record-updates-using-type-signatures/proposals/0000-disambiguating-record-updates-using-type-signatures.rst

Opinion is divided as to how important type-changing update is.  Thus we can
consider several alternative possibilities:

* Translate ``OverloadedRecordUpdate`` using ``SetField'`` so that it is not type-changing.
    This is simple but restrictive.  It means that enabling
    ``OverloadedRecordUpdate`` will break existing code that uses type-changing
    updates.  It is still useful to have the type-changing version available for
    optics libraries.  If we choose this option, we may wish to rename
    ``setField`` and ``setField'`` so that ``setField`` is non-type-changing and
    a different name is used for the type-changing version.

* Translate ``OverloadedRecordUpdate`` using ``SetField`` so that it allows (some) type-changing updates.
    This means users need to understand the rules around when ``SetField``
    constraints will be solved.  As the discussion above indicates, these rules
    will be nontrivial.  This will still not be completely backwards compatible
    as some type-changing updates permitted in traditional Haskell record update
    cannot be supported be ``SetField``.

* Introduce new syntax to distinguish type-changing from non-type-changing updates.
    This would be possible, but seems under-motivated. A related option
    would be to syntactically distinguish overloaded record updates from
    traditional record updates, which would have the merit of being a conservative
    extension (i.e. enabling ``OverloadedRecordUpdate`` would not break existing code).

* Introduce new syntax for performing an update while specifying the type being updated.
    See `proposal #310 <https://github.com/ghc-proposals/ghc-proposals/pull/310>`_.
    This is comparable to the ``DisambiguateRecordFields`` extension, which uses
    the data constructor in a record construction or pattern match to determine
    the type without need for type-directed field resolution.  This would make
    it possible to write type-changing updates (or other updates not supported
    by ``SetField``), but would not allow overloading.

* Allow field names to be qualified by the containing type name as if it was a
  module (essentially, use part of `proposal #283
  <https://github.com/ghc-proposals/ghc-proposals/pull/283>`_ on local modules).
  This is similar to the previous point but requires no new syntax.

In any case, users can choose to enable ``OverloadedRecordDot`` without
``OverloadedRecordUpdate``, meaning that dot notation for selection is
available, while updates are still treated in the traditional manner and may be
type-changing but not overloaded.  Users may also write out type-changing
updates explicitly (e.g. replacing ``t { foo = True }`` with ``case t of MkT{..}
-> MkT{foo=True, ..}``) or use an optics library.

Given the availability of these workarounds, and the greater simplicity and
predictability, we propose that ``OverloadedRecordUpdate`` **will not** permit
type-changing updates, as agreed in previous proposals.  Feedback from the
community and steering committee is particularly sought on this point, however.



Unresolved Questions
--------------------
TODO:

* Should ``OverloadedRecordUpdate`` permit type-changing update via ``SetField``?

* Is the proposed constraint-solving behaviour for ``SetField``
  satisfactory?



Implementation Plan
-------------------
TODO:
(Optional) If accepted who will implement the change? Which other resources
and prerequisites are required for implementation?
