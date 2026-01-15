package aggregate.nonfree

object Env:
  import aggregate.AggregateAPI.Device
  import aggregate.ValueTrees.*

  opaque type Env = Map[Device, ValueTree[Any]]

  object Env:
    private[nonfree] enum TreeNodeType:
      case XC
      case Call(id: String)
      case NVal

    def apply(env: Map[Device, ValueTree[Any]]): Env = env
    def apply(env: (Device, ValueTree[Any])*): Env = Map(env*)

  import Env.TreeNodeType
  extension (env: Env)
    private[nonfree] def alignWithChild(nChild: Int, nodeType: TreeNodeType): Env = 
        env
          .view
          .mapValues(_.children(nChild))
          .filter((_, t) =>
            // Since branches can only happen due to "Call"s i think it would
            // be enough to just check for those, but for now we check each
            // tree node.
            (t, nodeType) match
              case (t: ValueTree.XC[?, ?], TreeNodeType.XC) => true
              case (t: ValueTree.NVal[?], TreeNodeType.NVal) => true
              case (ValueTree.Call(id1, _, _), TreeNodeType.Call(id2))  => id1 == id2
              case _ => false
        ).toMap

    private[nonfree] def alignedDevices: Set[Device] = 
      env.keySet

    private[nonfree] def top: Map[Device, ValueTree[Any]] = 
      env
