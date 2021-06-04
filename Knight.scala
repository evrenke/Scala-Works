trait AttackStrategy
{
  def attack(opponent: Knight)
}

object Mace extends AttackStrategy
{
  override def attack(opponent: Knight): Unit = {
    println("macing")
    opponent.health -= 10
  }
}

object Stab extends AttackStrategy
{
  override def attack(opponent: Knight): Unit = {
    println("stabbing")
    opponent.health -= 15
  }
}

class Knight(val name: String)
{
  var health = 100
  var strategy: AttackStrategy = Stab

  def attack(opponent: Knight): Unit = {
    println(name + " attacking " + opponent.name)
    strategy.attack(opponent)
    println(opponent.name + ".health = " + opponent.health)
  }
}

object Knight
{
  def apply(n: String) = new Knight(n)
}

class CompositeStrategy(strategies: List[AttackStrategy]) extends AttackStrategy
{
  override def attack(opponent: Knight): Unit = {
    strategies.foreach(_.attack(opponent))
  }
}

object KnightBattle extends App {
  val k1 = new Knight("Drobot")
  val k2 = new Knight("Baldimore")
  k1.strategy = Mace
  k2.strategy = new CompositeStrategy(List(Mace, Stab, Stab))

  k1.attack(k2)
  // Drobot is attacking Baldimore
  // macing
  // Baldimore.health = 90

  k2.attack(k1)
  // Baldimore is attacking Drobot
  // macing
  // stabbing
  // stabbing
  // Drobot.health = 60
}