Non-overloaded updates with OverloadedRecordUpdate
==================================================

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

The ``OverloadedRecordUpdate`` extension changes the meaning of traditional
Haskell record update syntax.  However, it does not support some updates that
are possible with the traditional syntax.  This proposal introduces a fallback
mechanism, so that non-overloaded updates can be written even when
``OverloadedRecordUpdate`` is enabled.


Motivation
----------

The ``OverloadedRecordUpdate`` extension (introduced in `proposal #282
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0282-record-dot-syntax.rst>`_
as amended by `proposal #405
<https://github.com/ghc-proposals/ghc-proposals/pull/405>`_) changes the meaning
of record update syntax to use typeclass polymorphism, based on the ``setField``
method that is due to be added to the ``HasField`` typeclass following `proposal
#158
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0158-record-set-field.rst>`_.
For example, with ``OverloadedRecordUpdate`` the following update applies to any
type ``s`` with a field ``f :: Char``: ::

    {-# LANGUAGE OverloadedRecordUpdate #-}

    foo :: HasField "f" s Char => s -> s
    foo x = x { f = 'c' }

Notice that ``f`` need not be in scope at all, or there may be multiple fields
called ``f`` in scope.

Unfortunately, there are cases that cannot be handled by
``OverloadedRecordUpdate``, including:

* Type-changing update

* Updates to unlifted fields or data types

* Multiple updates, where several fields must be changed simultaneously for the
  update to be type-correct

* Updates to fields with higher-rank types

Some of these may be addressed by subsequent design changes to the ``HasField``
class, but others seem fundamentally difficult (in particular, multiple updates
and higher-rank types).  Thus there are cases where enabling
``OverloadedRecordUpdate`` is not a conservative extension, i.e. it will break
existing code.  For example, the following update is accepted by traditional
Haskell but is rejected when ``OverloadedRecordUpdate`` is enabled: ::

    {-# LANGUAGE OverloadedRecordUpdate, OverloadedRecordDot #-}
    module M where
      data Pair a = MkPair { x :: a, y :: a }

      bar :: Pair Int -> Pair Bool
      bar p = p { x = p.x > 0, y = p.y > 0 }

It would therefore be useful to have a way to mark specific updates as
non-overloaded, so that the traditional name resolution mechanism applies
instead.

There are various ways we could imagine to do this (e.g. adding some kind of
alternative syntax), but an appealing option that introduces no new syntax is to
look for the presence of a field with a qualified name in the update.  Such
fields are not currently allowed when ``OverloadedRecordUpdate`` is enabled, per
`section 2.1 of proposal #282
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0282-record-dot-syntax.rst#language-extensions>`_:

    If ``OverloadedRecordUpdate`` is on: ...

    * Update expressions with qualified labels like ``r{M.x = val}`` are disallowed.

Instead of failing entirely, we can interpret the presence of a qualified label
as selecting the use of non-overloaded update rather than overloaded update.


Proposed Change Specification
-----------------------------

If ``OverloadedRecordUpdate`` is enabled, then any update expression where at
least one of the fields being updated has a qualified name will be renamed and
type-checked as a traditional non-overloaded update (that is, as if
``OverloadedRecordUpdate`` was disabled).


Examples
--------

With this change, the following module will be accepted: ::

    {-# LANGUAGE OverloadedRecordUpdate, OverloadedRecordDot #-}
    module M where
      data Pair a = MkPair { x :: a, y :: a }

      bar :: Pair Int -> Pair Bool
      bar p = p { M.x = p.x > 0, y = p.y > 0 }

It is already permitted to qualify an identifier with the name of the module to
which it belongs.  Note that as long as at least one field is qualified, it is
not necessary to qualify all the fields.

Mixing updates in a single module
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This example demonstrates mixing overloaded and non-overloaded updates in a
single module, which is possible under this proposal but not the status quo: ::

    {-# LANGUAGE OverloadedRecordUpdate, DuplicateRecordFields, RankNTypes #-}

    module M where
      data S = MkS { f :: Bool }
      data T = MkT { f :: Int, poly :: forall a . a -> a }

      eg1 :: S -> S
      eg1 r = r { f = True }     -- relies on OverloadedRecordUpdate; f would be ambiguous

      eg2 :: T -> T
      eg2 r = r { f = 0 }        -- ditto

      eg3 :: T -> T
      eg3 r = r { M.poly = id }  -- relies on non-overloaded update


Effect and Interactions
-----------------------
``OverloadedRecordUpdate`` can now be used for both non-overloaded and
overloaded updates.

This change has no effect on any existing uses of ``OverloadedRecordUpdate``,
since qualified names are not currently allowed when the extension is enabled.


Costs and Drawbacks
-------------------
Users may find it confusing that even under ``OverloadedRecordUpdate``, some
updates are still non-overloaded.  `Section 7.6 of proposal #282
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0282-record-dot-syntax.rst#should-a-new-update-syntax-be-added>`_
reads:

    **Should a new update syntax be added?**

    One suggestion is that record updates remain as normal, but ``a { .foo = 1
    }`` be used to indicate the new forms of updates. While possible, we believe
    that option leads to a confusing result, with two forms of update both of
    which fail in different corner cases. Instead, we recommend use of
    ``C{foo}`` as a pattern (with ``-XNamedFieldPuns``) to extract fields if
    necessary.

While this refers to new syntax for overloaded updates, the same argument
applies if the distinction is based on the presence of a module qualifier as
proposed here.  Arguably a design with two forms of update is more complex and
confusing than one.

In practice, however, this problem cannot be avoided while the
``OverloadedRecordUpdate`` extension exists: non-overloaded updates are unlikely
to be completely displaced by overloaded updates, so programmers will still
encounter both in the wild, and there will inevitably be corner cases where one
or the other is preferable.


Alternatives
------------
Having a different syntax for overloaded and non-overloaded updates would make
it more obvious that they are treated differently.
However, if ``OverloadedRecordUpdate`` is to be given a different syntax, a
slightly more radical alternative would be to remove it altogether and instead
write overloaded updates using (a cut down version of) an optics library: For
example, the following is already possible: ::

    {-# LANGUAGE OverloadedLabels #-}

    import Optics

    foo :: HasField "f" s Char => s -> s
    foo x = x & #f .~ 'c'

TODO: think about this more


Unresolved Questions
--------------------
None.


Implementation Plan
-------------------
TODO: If accepted who will implement the change? Which other resources
and prerequisites are required for implementation?
