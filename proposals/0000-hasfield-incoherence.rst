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
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0023-overloaded-record-fields.rst#virtual-record-fields>`,
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

TODO: More motivating examples.

Proposed Change Specification
-----------------------------
All four restrictions are removed.

Examples
--------
TODO: How do we write this without duplicating motivation? Or is that
duplication OK?

Effect and Interactions
-----------------------
Your proposed change addresses the issues raised in the motivation. Explain how.

Also, discuss possibly contentious interactions with existing language or compiler
features. Complete this section with potential interactions raised
during the PR discussion.

Costs and Drawbacks
-------------------
The minmial implementation cost should simply remove some code, so in one sense
this change is a simplification. However, the implementation cost may latter
rise if we want to provide more informative error messages. For now, it's
suggested that we just emit the traditional overlapping instances error
messages.

A perhaps more significant cost/drawback of this proposal is that it permits
record fields to be incoherent.

TODO Expand on/emphasise this

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

