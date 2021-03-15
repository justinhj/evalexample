

object ReaderPlay extends App:

  import org.justinhj.typeclasses.monad.{given,_}
    
  // ReaderT data type
  // Mostly from https://github.com/scalaz/scalaz/blob/80ba9d879b4f80f0175b5f904ac4587b02400251/core/src/main/scala/scalaz/Kleisli.scala
  
  case class ReaderT[F[_],R,A](run: R => F[A]):
    // TODO understand usage of this 
    def local[RR](f: RR => R): ReaderT[F, RR, A] = ReaderT(f.andThen(run))
  
  // Companion object 
  
  object ReaderT:
    //def apply[F[_],R,A](run: R => F[A]): ReaderT[F,R,A] = ReaderT(run)
    // TODO Untested 
    def lift[F[_],R,A](fa: F[A]): ReaderT[F,R,A] = ReaderT(_ => fa) 

  // Monad instance
        
  given readerTMonad[F[_] : Monad,R]: Monad[[A1] =>> ReaderT[F,R,A1]] with
    def pure[A](a: A): ReaderT[F,R,A] = ReaderT(_ => Monad[F].pure(a))
  
    extension [A,B](fa: ReaderT[F,R,A]) 
      def flatMap(f: A => ReaderT[F,R,B]) =
        val r2ReaderFRB = (r: R) => fa.run(r).flatMap(b => f(b).run(r))
        ReaderT(r2ReaderFRB)
  
  // Domain from Typelevel docs
  // https://typelevel.org/cats/datatypes/kleisli.html
  case class DbConfig(url: String, user: String, pass: String)
  
  class Db
  object Db {
    val fromDbConfig: ReaderT[Option, DbConfig, Db] = ReaderT((config: DbConfig) => Option(new Db))
  }

  // Option(DbConfig("db.com","root","yolo"))
  
  case class ServiceConfig(addr: String, port: Int)
  class Service
  object Service {
    val fromServiceConfig: ReaderT[Option, ServiceConfig, Service] = ReaderT((config: ServiceConfig) => Option(new Service))
  }

  case class AppConfig(dbConfig: DbConfig, serviceConfig: ServiceConfig)

  case class Application(db: Db, service: Service)

  def appFromAppConfig: ReaderT[Option, AppConfig, Application] =
    for {
      db <- Db.fromDbConfig.local[AppConfig](_.dbConfig)
      sv <- Service.fromServiceConfig.local[AppConfig](_.serviceConfig)
    } yield new Application(db, sv)
    
  val app = appFromAppConfig.run(
      AppConfig(DbConfig("db.com","root","yolo"),
          ServiceConfig("service.com",2020)))
      
  app.foreach(a => 
      println(s"app $a, config ${a.db}")
  )
