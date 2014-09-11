package demo

import akka.actor._
import scala.concurrent.duration._

class Ball(val number: Int) extends Actor {
  def receive = {
    case "good_bye" =>
      println(number + ": Good Bye!") //return itself back to the floor

    case "fly_now" =>
      println(number + ": Hey, I'am flying!")
      Thread.sleep(2000 + scala.util.Random.nextInt(1000)) //???? system.scheduler.scheduleOnce could be used too, but worker Actor can block
      println(number + ": Now catch me!")
      sender ! ("catch_me", number)
  }
}

class Juggler(var number_of_throws: Int) extends Actor {
  var number_of_balls_in_air = 0
  import context._ //???? is there a better place for it?
  def receive = {
    case ("add", ball: Int) => //Floor requested to add a new ball to circulation
      if (number_of_throws > 0) {
        number_of_throws = number_of_throws - 1
        val x = number_of_throws
        system.scheduler.scheduleOnce(scala.util.Random.nextInt(1000) milliseconds) { //???? cannot block in the control actor
          println("first throw #" + x + " for ball: " + ball)
          number_of_balls_in_air = number_of_balls_in_air + 1
          context.actorOf(Props(new Ball(ball))) ! "fly_now"//???? is it really best place to create Actor Ball, is there a shortcut for this: context.actorOf(Props(new Ball(ball)))
        }
      }

    case ("catch_me", ball: Int) =>
      if (number_of_throws > 0) { //???? fix code repetition: "number_of_throws>0" (here and above)
        number_of_throws = number_of_throws - 1
        println("rethrowing ball #" + ball + ", number of throws to go: " + number_of_throws)
        sender ! "fly_now"
      } else {
        number_of_balls_in_air = number_of_balls_in_air - 1
        println("dropping ball #" + ball + ", still left in air:" + number_of_balls_in_air)
        sender ! "good_bye"
        if (number_of_balls_in_air == 0)
          context.system.shutdown
      }
  }
}

class Floor extends Actor {
  def receive = {
    case (num_of_balls: Int, num_of_throws: Int) =>
      val juggler = context.actorOf(Props(new Juggler(num_of_throws)))
      (1 to num_of_balls).foreach(f => juggler ! ("add", f)) // initiate juggling
  }
}

object Main extends App {
  val system = ActorSystem("Juggler")
  system.actorOf(Props(new Floor)) ! (5, 50)
  system.awaitTermination
  println("DONE")
}
