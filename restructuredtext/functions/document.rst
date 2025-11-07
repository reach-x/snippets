=====================================
ReStructuredText (RST) Example
=====================================

:Author: John Doe
:Date: 2025-01-07
:Version: 1.0.0
:Copyright: Copyright (C) 2025
:Abstract: This document demonstrates reStructuredText markup features

.. contents:: Table of Contents
   :depth: 3

Introduction
============

This is a paragraph in **reStructuredText**. RST is a lightweight markup
language that is easy to read and write. It is commonly used for Python
documentation with Sphinx.

Basic Text Formatting
=====================

Inline Markup
-------------

- *Italic text* (emphasis)
- **Bold text** (strong emphasis)
- ``Inline code`` (code samples)
- *Can be* **combined** ``together``

Subscript and Superscript
~~~~~~~~~~~~~~~~~~~~~~~~~~

- H\ :sub:`2`\ O (water)
- E = mc\ :sup:`2` (Einstein's equation)

Headers
=======

Level 1 Header
--------------

Level 2 Header
~~~~~~~~~~~~~~

Level 3 Header
^^^^^^^^^^^^^^

Level 4 Header
""""""""""""""

Lists
=====

Bullet Lists
------------

- First item
- Second item

  - Nested item 1
  - Nested item 2

- Third item

Enumerated Lists
----------------

1. First numbered item
2. Second numbered item

   a. Nested letter item
   b. Another letter item

3. Third numbered item

#. Auto-numbered item
#. Another auto-numbered item

Definition Lists
----------------

Term 1
    Definition of term 1

Term 2
    Definition of term 2 with more details.
    Can span multiple lines.

Field Lists
-----------

:Date: 2025-01-07
:Author: John Doe
:Version: 1.0.0
:Status: Draft

Option Lists
------------

-a            Output all files
-b            Binary mode
--long        Long option
--input=file  Option with argument

Code Blocks
===========

Literal Blocks
--------------

This is a literal block::

    def hello():
        print("Hello, World!")
        return True

Code Directive
--------------

.. code-block:: python
   :linenos:
   :emphasize-lines: 2,3

   def factorial(n):
       if n <= 1:
           return 1
       return n * factorial(n - 1)

.. code-block:: javascript
   :caption: JavaScript Example

   function greet(name) {
       console.log(`Hello, ${name}!`);
   }

Tables
======

Grid Tables
-----------

+------------+------------+-----------+
| Header 1   | Header 2   | Header 3  |
+============+============+===========+
| Row 1, Col | Row 1, Col | Row 1,    |
| 1          | 2          | Col 3     |
+------------+------------+-----------+
| Row 2, Col | Row 2, Col | Row 2,    |
| 1          | 2          | Col 3     |
+------------+------------+-----------+

Simple Tables
-------------

=====  =====  ======
   Inputs     Output
------------  ------
  A      B    A or B
=====  =====  ======
False  False  False
True   False  True
False  True   True
True   True   True
=====  =====  ======

CSV Table
---------

.. csv-table:: Programming Languages
   :header: "Language", "Year", "Type"
   :widths: 20, 10, 20

   "Python", "1991", "Interpreted"
   "Java", "1995", "Compiled"
   "JavaScript", "1995", "Interpreted"

List Table
----------

.. list-table:: Features Comparison
   :widths: 25 25 50
   :header-rows: 1

   * - Feature
     - Available
     - Notes
   * - Syntax Highlighting
     - Yes
     - Via Pygments
   * - Math Support
     - Yes
     - Via Sphinx extensions

Links and References
====================

External Links
--------------

Visit `Python's website <https://www.python.org>`_.

Anonymous hyperlinks: `Google`__ and `GitHub`__.

__ https://www.google.com
__ https://www.github.com

Internal References
-------------------

See the `Introduction`_ section above.

Jump to `Code Blocks`_.

.. _custom-label:

Custom Label Target
^^^^^^^^^^^^^^^^^^^

Link to `custom label <custom-label_>`_.

Footnotes
---------

This is a statement with a footnote [#f1]_.

Another statement with a footnote [#f2]_.

.. [#f1] This is the first footnote.
.. [#f2] This is the second footnote with more details.

Citations
---------

According to research [PEP287]_.

.. [PEP287] Goodger, D. "reStructuredText Docstring Format"
   Python Enhancement Proposal 287.
   https://www.python.org/dev/peps/pep-0287/

Images and Figures
==================

Simple Image
------------

.. image:: /path/to/image.png
   :width: 200px
   :alt: Alternative text
   :align: center

Figure with Caption
-------------------

.. figure:: /path/to/figure.png
   :scale: 50%
   :align: center
   :alt: Figure caption

   This is the caption for the figure.
   It can span multiple lines.

Admonitions
===========

.. note::
   This is a note admonition.
   It highlights important information.

.. warning::
   This is a warning! Be careful.

.. danger::
   This indicates a dangerous situation.

.. tip::
   This is a helpful tip for readers.

.. important::
   This is important information.

.. hint::
   Here's a hint to help you.

.. attention::
   Pay attention to this!

.. caution::
   Proceed with caution.

.. error::
   An error has occurred.

Custom Admonition
-----------------

.. admonition:: Custom Title
   :class: custom-css-class

   This is a custom admonition with a custom title.

Directives
==========

Contents Directive
------------------

.. contents:: Section Contents
   :local:
   :depth: 2

Include Directive
-----------------

.. include:: other-file.rst

Raw Directive
-------------

.. raw:: html

   <div style="color: red;">
       This is raw HTML content.
   </div>

Math Directive
--------------

.. math::

   \int_0^\infty e^{-x^2} dx = \frac{\sqrt{\pi}}{2}

Inline Math
-----------

The equation :math:`E = mc^2` is Einstein's mass-energy equivalence.

Substitutions
=============

.. |RST| replace:: reStructuredText
.. |date| date::
.. |time| date:: %H:%M

This document demonstrates |RST| features.

Generated on |date| at |time|.

Comments
========

.. This is a comment that won't appear in the output.

..
   This is a multi-line comment.
   It also won't appear in the output.

Roles
=====

Sphinx-specific Roles
---------------------

- :py:func:`function_name`
- :py:class:`ClassName`
- :py:mod:`module.name`
- :ref:`reference-label`
- :doc:`document-name`
- :download:`file.pdf`

Generic Roles
-------------

- :emphasis:`emphasized text`
- :strong:`strong text`
- :literal:`literal text`
- :subscript:`subscript`
- :superscript:`superscript`
- :title-reference:`Book Title`

Transitions
===========

A transition is a horizontal line:

----

It separates sections.

Doctest Blocks
==============

>>> print("Hello, World!")
Hello, World!
>>> 2 + 2
4

Line Blocks
===========

| This is a line block.
| Each line begins with a vertical bar.
| Whitespace is preserved.
|     Indentation is also preserved.

Block Quotes
============

This is a normal paragraph.

    This is a block quote.
    It is indented.

        This is a nested block quote.

-- Attribution

Conclusion
==========

This document covered most of the reStructuredText syntax features.
RST is powerful for technical documentation, especially with Sphinx.

.. seealso::

   - `reStructuredText Primer <https://www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html>`_
   - `Docutils Documentation <https://docutils.sourceforge.io/rst.html>`_
