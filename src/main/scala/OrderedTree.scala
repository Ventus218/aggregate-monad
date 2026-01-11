package orderedtree

object OrderedTree:
  case class OrderedTree[+A](value: A, children: Seq[OrderedTree[A]] = Seq())

  // Each element is the child to follow (empty list means current node)
  opaque type Cursor = List[Int]

  opaque type LeftToRightOrderedTreeBuilder[A] = Builder[A]
  private case class Builder[A](tree: OrderedTree[A], cursor: Cursor)

  object LeftToRightOrderedTreeBuilder:
    // TODO: check performance and stack safe recursion

    import OrderedTree.*

    def root[A](value: A): LeftToRightOrderedTreeBuilder[A] =
      Builder(OrderedTree(value, Seq()), List())

    extension [A](b: LeftToRightOrderedTreeBuilder[A])
      private def throwCursorDoesNotBelongToTree: Nothing =
        throw IllegalArgumentException("cursor doesn't belong to tree")

      /** Appends a new child to the node pointed by the cursor and moves the
        * cursor to that new child
        */
      def enter(value: A): LeftToRightOrderedTreeBuilder[A] =
        def helper(
            t: OrderedTree[A],
            cursor: Cursor
        ): (OrderedTree[A], Cursor) =
          cursor match
            case Nil =>
              val res =
                t.copy(children = t.children :+ OrderedTree(value, Seq()))
              (res, b.cursor :+ res.children.length)
            case index :: next =>
              t.children.lift(index) match
                case None => throwCursorDoesNotBelongToTree
                case Some(child) =>
                  val (newChild, newCursor) = helper(child, next)
                  (
                    t.copy(children = t.children.updated(index, newChild)),
                    newCursor
                  )
        val (newTree, newCursor) = helper(b.tree, b.cursor)
        Builder(newTree, newCursor)

      /** Sets the node pointed by cursor to value before moving the cursor up
        * to its parent
        */
      def exit(value: A): LeftToRightOrderedTreeBuilder[A] =
        b.setValue(value).exit

      /** Moves the cursor up to its parent */
      def exit: LeftToRightOrderedTreeBuilder[A] =
        require(b.cursor.length > 0, "can't exit, cursor already at root")
        Builder(b.tree, b.cursor.dropRight(1))

      /** Sets the node pointed by cursor to value.
        *
        * Expects the cursor to not be set on an empty tree
        */
      def setValue(value: A): LeftToRightOrderedTreeBuilder[A] =
        def helper(t: OrderedTree[A], cursor: Cursor): OrderedTree[A] =
          cursor match
            case Nil =>
              t.copy(value = value)
            case index :: next =>
              t.children.lift(index) match
                case None => throwCursorDoesNotBelongToTree
                case Some(child) =>
                  t.copy(children =
                    t.children.updated(index, helper(child, next))
                  )
        b.copy(tree = helper(b.tree, b.cursor))

      def valueAtCursor: A =
        def helper(t: OrderedTree[A], cursor: Cursor): A =
          cursor match
            case Nil => t.value
            case index :: next =>
              t.children.lift(index) match
                case None        => throwCursorDoesNotBelongToTree
                case Some(child) => helper(child, next)
        helper(b.tree, b.cursor)

      def tree: OrderedTree[A] = b.tree
      def cursor: Cursor = b.cursor
