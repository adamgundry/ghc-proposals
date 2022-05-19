Proposal title
==============

.. author:: Ollie Charles
.. date-accepted::
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. contents::

In the `overloaded record fields proposal
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0023-overloaded-record-fields.rst>`_,
there is a `set of limitations
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0023-overloaded-record-fields.rst#virtual-record-fields>`_
as to when a user can declare custom ``HasField`` instances. In this proposal
we relax these restrictions, allowing users to define ``HasField`` instances
when they were previously unable to.

Motivation
----------
The current conditions prohibit a user from defining ``HasField`` custom
instances under a few conditions. This proposal argues that these constraints
are excessively restrictive and that GHC would benefit from relaxing these
constraints.

These conditions can be found in the `"Virtual record fields" section of
proposal 0023
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0023-overloaded-record-fields.rst#virtual-record-fields>`_,
and these conditions are:

* ``HasField _ r _`` where ``r`` is a variable;

* ``HasField _ (T ...) _`` if ``T`` is a data family (because it might have
  fields introduced later, using data instance declarations);

* ``HasField x (T ...) _`` if ``x`` is a variable and ``T`` has any fields at
  all (but this instance is permitted if ``T`` has no fields);

* ``HasField "foo" (T ...) _`` if ``T`` has a field ``foo`` (but this instance
  is permitted if it does not).

These restrictions are in place to guarantee coherence, but come at the cost of
ruling out a variety of productive instances. In this section, we will consider
a few examples of where we might wish to add fields, but are currently denied
this opportunity.

Example 1
~~~~~~~~~

Consider the newtype::

  newtype Behavior a = Behavior { unB :: ... }

This type represents a time-varying value, and occurs in the
``reactive-banana`` project. A ``Behavior`` is a ``Functor``, and it would be
nice if we could "lift" any fields in ``a`` to ``Behavior a``::

  instance HasField x a b => HasField (x :: k) (Behavior a) b where
    getField b = getField @x <$> b

Unfortunately, because ``Behavior`` was defined as ``{ unB :: ... }``, GHC
considers it to have fields, and we are unable to write the ``HasField``
instance we want. Worse, this is an abstraction leak - users of
``reactive-banana`` can only see ``data Behavior a`` - its constructor and
fields are not exported. This leads to a confusing error message if a user
tries to define the above custom (orphan) instance, as they are told the type
has fields, though without reading the source they are otherwise unable to
observe that.

This was originally reported as `GHC issue #21324 <https://gitlab.haskell.org/ghc/ghc/-/issues/21324>`_.
A similar request is at `issue #21369 <https://gitlab.haskell.org/ghc/ghc/-/issues/21369>`_.

TODO: More motivating examples.

Proposed Change Specification
-----------------------------
All four restrictions are removed.

``HasField`` constraints have `special treatment in the constraint solver
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0023-overloaded-record-fields.rst#solving-hasfield-constraints>`_.
Currently, the constraint solver is able to ignore the possibility that
user-defined ``HasField`` instances will overlap with those generated
internally, because of the restrictions on user-defined instances.

With the change proposed here, it will be legal to define instances of
``HasField`` just like any other class, ignoring its special-purpose constraint
solving behaviour.  Instead, during constraint solving, the compiler will check
for overlapping instances at use sites and report errors, just as it does for
other typeclasses.



Examples
--------
TODO: How do we write this without duplicating motivation? Or is that
duplication OK?

AMG: I think the idea is that the examples section can pick trivial/synthetic
examples that illustrate the specification.

For example, the following is accepted::

  module M where
    data T = MkT { foo :: Int }

    instance HasField T "foo" Bool where
      getField (MkT x) = x > 0

  module N where
    import M (T)

    bar :: T -> Bool
    bar = getField @"foo"

The ``HasField T "foo" Bool`` constraint arising from the call to ``getField``
in ``bar`` is solved using the user-defined instance.  The qualified import
means that the ``foo`` field is not in scope, so the built-in constraint solving
behaviour does not apply.

If the definition of ``bar`` was in module ``M`` instead, or if module ``N`` had
an unqualified import of module ``M``, an overlapping instance error would be
reported because the ``foo`` field would be in scope and would conflict with the
user-defined ``HasField`` instance.



Effect and Interactions
-----------------------

This change allows incoherent solutions to ``HasField`` constraints: two
``HasField`` constraints solved in different modules may be instantiated with
different dictionaries.  During optimization, the compiler may not assume that
two ``HasField`` dictionaries of the same type have the same value.  (This is
similar to the ``IP`` class underlying the implementation of implicit
parameters.)

For example::

  module M where
    data T = MkT { foo :: Int }

    bar = getField @"foo" (MkT 42)

  module N where
    import M (T(MkT))

    instance HasField T "foo" Int where
      getField (MkT x) = negate x

    baz = getField @"foo" (MkT 42)

Here ``bar`` evaluates to ``42`` but ``baz`` evaluates to ``-42``, despite
having apparently the same definition.

Incoherence occurs only in the presence of orphan instances, because for a
user-defined ``HasField`` instance to be non-orphan it must be defined in the
same module as the record datatype.



Costs and Drawbacks
-------------------

The implementation cost of this proposal is likely to be small (removing the
code for the check and adjusting the constraint solver to check for overlapping
``HasField`` instances).  However, the implementation cost may later rise if we
want to provide more informative error messages. For now, we propose that the
compiler just emit the traditional overlapping instances error messages.

Accepting incoherence for ``HasField`` instances may reduce optimization
opportunities in some cases, because the compiler will not be able to assume
that two dictionaries of the same type have the same value.


Alternatives
------------
Rather than relaxing all four restrictions at once, we could instead remove restrictions as requested. Such a strategy may lead to smaller changes, but on the other hand may lead to more changes. Perhaps it is better to make sweeping changes to a new feature while it's settling, rather than having developers try and develop against a moving target.

Unresolved Questions
--------------------
TODO: Are there any?

Implementation Plan
-------------------
Ollie Charles has offered to help implement this proposal if it is accepted.

Endorsements
-------------

