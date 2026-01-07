package orderedtree

object OrderedTree:
  enum OrderedTree[+A]:
    case Empty
    case Node(value: A, child: OrderedTree[A], sibling: OrderedTree[A])

  object OrderedTree:
    extension [A](t: OrderedTree[A])
      def children: Seq[OrderedTree[A]] =
        t match
          case Empty             => Seq()
          case Node(_, Empty, _) => Seq()
          case Node(_, child, _) => Seq(child) ++ child.siblings

      def siblings: Seq[OrderedTree[A]] =
        t match
          case Empty               => Seq()
          case Node(_, _, Empty)   => Seq()
          case Node(_, _, sibling) => Seq(sibling) ++ sibling.siblings

  private enum Choice:
    case Child
    case Sibling

  // The cursor position is the list of choices made starting from the root node
  opaque type Cursor = List[Choice]
  extension (c: Cursor)
    def nextChild: Cursor = c :+ Choice.Child
    def nextSibling: Cursor = c :+ Choice.Sibling
    def parent: Cursor =
      // TODO: improve performance
      c.reverse.dropWhile(_ == Choice.Sibling).drop(1).reverse
    def path: Seq[Cursor] =
      c.foldLeft(List[Cursor]())((list, choice) =>
        list match
          case Nil  => List(List(choice))
          case list => list :+ (list.last :+ choice)
      )

  opaque type LeftToRightOrderedTreeBuilder[A] = Builder[A]
  private case class Builder[A](tree: OrderedTree[A], cursor: Cursor)

  object LeftToRightOrderedTreeBuilder:
    // TODO: check performance and stack safe recursion

    import OrderedTree.*
    import Choice.*

    def root[A](value: A): LeftToRightOrderedTreeBuilder[A] =
      Builder(Node(value, Empty, Empty), List())

    extension [A](b: LeftToRightOrderedTreeBuilder[A])
      /** Appends a new child to the node pointed by the cursor and moves the
        * cursor to that new child
        */
      def enter(value: A): LeftToRightOrderedTreeBuilder[A] =
        val newTree =
          b.tree.appendChildAtCursor(Node(value, Empty, Empty), b.cursor)
        val newCursor = (b.cursor :+ Child) ++ newTree
          .subtreeAtCursor(b.cursor)
          .children
          .drop(1)
          .foldLeft(List.empty[Choice])((c, _) => c :+ Sibling)
        Builder(newTree, newCursor)

      /** Sets the node pointed by cursor to value before moving the cursor up
        * to its parent
        */
      def exit(value: A): LeftToRightOrderedTreeBuilder[A] =
        b.setValue(value).exit

      /** Moves the cursor up to its parent */
      def exit: LeftToRightOrderedTreeBuilder[A] =
        Builder(b.tree, b.cursor.parent)

      /** Sets the node pointed by cursor to value.
        *
        * Expects the cursor to not be set on an empty tree
        */
      def setValue(value: A): LeftToRightOrderedTreeBuilder[A] =
        def helper(t: OrderedTree[A], cursor: Cursor): OrderedTree[A] =
          t match
            case Empty =>
              require(cursor.isEmpty, "cursor doesn't match the tree")
              throw IllegalArgumentException("Cannot set value on empty node")
            case Node(v, child, sibling) =>
              cursor match
                case Nil          => Node(value, child, sibling)
                case Child :: t   => Node(v, helper(child, t), sibling)
                case Sibling :: t => Node(v, child, helper(sibling, t))
        b.copy(tree = helper(b.tree, b.cursor))

      def tree: OrderedTree[A] = b.tree
      def cursor: Cursor = b.cursor

    extension [A](t: OrderedTree[A])
      /** Expects a vaild cursor for appending (not inserting) */
      private def appendSiblingAtCursor(
          s: OrderedTree[A],
          cursor: Cursor
      ): OrderedTree[A] =
        t match
          case Empty =>
            require(cursor.isEmpty, "cursor doesn't match the tree")
            s
          case Node(value, child, sibling) =>
            cursor match
              case Nil =>
                Node(value, child, sibling.appendSiblingAtCursor(s, Nil))
              case Choice.Child :: t =>
                Node(value, child.appendSiblingAtCursor(s, t), sibling)
              case Choice.Sibling :: t =>
                Node(value, child, sibling.appendSiblingAtCursor(s, t))

      /** Expects a vaild cursor for appending (not inserting) */
      private def appendChildAtCursor(
          c: OrderedTree[A],
          cursor: Cursor
      ): OrderedTree[A] =
        t match
          case Empty =>
            throw IllegalArgumentException(
              "Can't append a child to an Empty tree"
            )
          case Node(value, child, sibling) =>
            cursor match
              case Nil =>
                Node(value, child.appendSiblingAtCursor(c, Nil), sibling)
              case Choice.Child :: t =>
                Node(value, child.appendChildAtCursor(c, t), sibling)
              case Choice.Sibling :: t =>
                Node(value, child, sibling.appendChildAtCursor(c, t))

      private def subtreeAtCursor(cursor: Cursor): OrderedTree[A] =
        t match
          case Empty =>
            require(cursor.isEmpty, "cursor doesn't match the tree")
            Empty
          case Node(value, child, sibling) =>
            cursor match
              case Nil                 => t
              case Choice.Child :: t   => child.subtreeAtCursor(t)
              case Choice.Sibling :: t => sibling.subtreeAtCursor(t)
