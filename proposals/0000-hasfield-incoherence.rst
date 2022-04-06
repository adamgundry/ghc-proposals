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
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0023-overloaded-record-fields.rst>,
there is a `set of limitations
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0023-overloaded-record-fields.rst#virtual-record-fields>`
as to when a user can declare custom ``HasField`` instances. In this proposal
these restrictions, allowing users to define ``HasField`` instances even when
the original data type has fields.

Motivation
----------
The current conditions prohibit a user from defining ``HasField`` instances if
the type in question has fields defined. This restriction is in place to
guarantee coherence, but is very restrictive. In this section, we will consider
a few examples of where we might wish to add fields, but are currently denied
this opportunity.

First, consider the newtype::

  newtype Behavior a = Behavior { unB :: ... }

This type represents a time-varying value, and occurs in the
``reactive-banana`` project. A ``Behavior`` is a ``Functor``, and it would be
quite nice if we could "lift" any fields in ``a`` to ``Behavior a``::

  instance HasField x a b => HasField (x :: k) (Behavior a) b where
    getField b = getField @x <$> b

Unfortunately, because ``Behavior`` was defined as ``{ unB :: ... }``, GHC
considers it to have fields, and we are unable to write the ``HasField``
instance we want. Worse, this is an abstraction leak - users of
``reactive-banana`` can only see ``data Behavior a`` - its constructor and
fields are not exported.

TODO: More motivating examples.

Proposed Change Specification
-----------------------------
The list of reasons to prevent a ``HasField`` instance from being defined is
redefined as:

 * ``HasField _ r _`` where ``r`` is a variable;

 * ``HasField _ (T ...) _`` if ``T`` is a data family (because it
   might have fields introduced later, using data instance declarations);

 * ``HasField "foo" (T ...) _`` if ``T`` has a field ``foo`` (but this
   instance is permitted if it does not).

That is,

 * ``HasField x (T ...) _`` if ``x`` is a variable and ``T`` has any
   fields at all (but this instance is permitted if ``T`` has no fields);

is removed from the list of restrictions.

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
message.

A drawback of this proposal is that now allows users to experience incoherence
in field resolution. TODO: Expand on/emphasise this?

Alternatives
------------
TODO: What are the alternatives?

Unresolved Questions
--------------------
TODO: Are there any?

Implementation Plan
-------------------
Ollie Charles has offered to help implement this proposal if it is accepted.

Endorsements
-------------
(Optional) This section provides an opportunity for any third parties to express their
support for the proposal, and to say why they would like to see it adopted.
It is not mandatory for have any endorsements at all, but the more substantial
the proposal is, the more desirable it is to offer evidence that there is
significant demand from the community.  This section is one way to provide
such evidence.

