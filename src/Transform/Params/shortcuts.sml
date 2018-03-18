structure Shortcuts =
  struct
    val eC = Fdm.element
    fun eC2 e a v = eC e a (Fdm.concatForests v)
  end