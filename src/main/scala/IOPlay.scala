object IOPlay extends App:
  
  // This is the Scala 3 implementation of the IO monad 
  // based roughly on the red book
  
  enum IO[A]:
    case Return(a: A) extends IO[A]
    case Suspend(resume: () => A) extends IO[A]
    case FlatMap[A,B](fa: IO[B], k: B => IO[A]) extends IO[A]

    def flatMap[B](f: A => IO[B]): IO[B] =
      FlatMap(this, f)

    def map[B](f: A => B): IO[B] =
      flatMap(a => Return(f(a)))

  object IO:
    def delay[A](io: => A): IO[A] = Suspend(() => io)
  
  import IO._

  def putStrLn(s: String): IO[Unit] = 
    IO.delay(println(s))

  def getStrLn(): IO[String] =
    IO.delay(scala.io.StdIn.readLine())
  
  extension [A](io: IO[A])
    def run(): A =
      io match
        case Return(a) => a
        case Suspend(resume) =>
          resume()
        case FlatMap(x, f) => x match
          case Return(a) => f(a).run()
          case Suspend(r) => f(r()).run()
          case FlatMap(y,g) => (y flatMap (a => g(a) flatMap f)).run()
  
  val prog1 = for (
    _ <- putStrLn("Please tell me your name:");
    name <- getStrLn();
    _ <- putStrLn(s"Hello, $name!")
  ) yield ()

  prog1.run()