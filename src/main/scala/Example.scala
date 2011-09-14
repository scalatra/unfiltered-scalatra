package com.example

import _root_.org.scalatra.{RouteMatcher, Route}
import unfiltered.request._
import unfiltered.response._
import org.clapper.avsl.Logger
import util._
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import matching.Regex
import org.scalatra._

import java.lang.{Integer => JInteger}
import javax.servlet.ServletContext
import javax.servlet.http.{HttpServletRequest, HttpServletResponse, HttpSession}
import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.collection.mutable.ConcurrentMap
import java.util.concurrent.ConcurrentHashMap
import ScalatraKernel.{Action, MultiParams}
import util.{MapWithIndifferentAccess, MultiMapHeadView, MultiMap}

trait CoreDsl {
  
  type UnfilteredErrorHandler = PartialFunction[scala.Throwable, ResponseFunction[Any]]
  def params: Map[String, String]

  def multiParams: MultiParams

  def redirect(uri: String) = throw new RuntimeException("TODO") // TODO: provide redirect etc methods

  def before(routeMatchers: RouteMatcher*)(block: => ResponseFunction[Any]): Unit

  def after(routeMatchers: RouteMatcher*)(block: => ResponseFunction[Any]): Unit

  def notFound(block: => ResponseFunction[Any]): Unit

  def methodNotAllowed(block: Set[HttpMethod] => ResponseFunction[Any]): Unit

  def error(handler: UnfilteredErrorHandler): Unit

  def get(routeMatchers: RouteMatcher*)(block: => ResponseFunction[Any]): UnfilteredRoute[Any]

  def post(routeMatchers: RouteMatcher*)(block: => ResponseFunction[Any]): UnfilteredRoute[Any]

  def put(routeMatchers: RouteMatcher*)(block: => ResponseFunction[Any]): UnfilteredRoute[Any]

  def delete(routeMatchers: RouteMatcher*)(block: => ResponseFunction[Any]): UnfilteredRoute[Any]

  def options(routeMatchers: RouteMatcher*)(block: => ResponseFunction[Any]): UnfilteredRoute[Any]

  def patch(routeMatchers: RouteMatcher*)(block: => ResponseFunction[Any]): UnfilteredRoute[Any]

  def halt(status: JInteger = null,
           body: String = "",
           headers: Map[String, String] = Map.empty,
           reason: String = null): Nothing

  def pass()
}


case class UnfilteredRoute[T](
  routeMatchers: Iterable[RouteMatcher],
  action: () => ResponseFunction[T],
  contextPath: () => String = () => ""
)
{
  def apply(): Option[MatchedUnfilteredRoute[T]] = {
    routeMatchers.foldLeft(Option(MultiMap())) {
      (acc: Option[MultiParams], routeMatcher: RouteMatcher) => for {
        routeParams <- acc
        matcherParams <- routeMatcher()
      } yield routeParams ++ matcherParams
    } map { routeParams => MatchedUnfilteredRoute(action, routeParams) }
  }

  lazy val reversibleMatcher: Option[RouteMatcher] =
    routeMatchers find (_.isInstanceOf[ReversibleRouteMatcher])

  lazy val isReversible: Boolean = !reversibleMatcher.isEmpty

  override def toString: String = routeMatchers mkString " "
}

case class MatchedUnfilteredRoute[T](action: () => ResponseFunction[T], multiParams: MultiParams)


class UnfilteredRouteRegistry[T] {
  private val methodRoutes: ConcurrentMap[HttpMethod, Seq[UnfilteredRoute[T]]] =
    new ConcurrentHashMap[HttpMethod, Seq[UnfilteredRoute[T]]]

  private var _beforeFilters: Seq[UnfilteredRoute[T]] = Vector.empty
  private var _afterFilters: Seq[UnfilteredRoute[T]] = Vector.empty

  def apply(method: HttpMethod): Seq[UnfilteredRoute[T]] =
    method match {
      case Head => methodRoutes.getOrElse(Get, Vector.empty)
      case m => methodRoutes.getOrElse(m, Vector.empty)
    }

  def matchingMethods: Set[HttpMethod] = matchingMethodsExcept { _ => false }

  def matchingMethodsExcept(method: HttpMethod): Set[HttpMethod] = {
    val p: HttpMethod => Boolean = method match {
      case Get | Head => { m => m == Get || m == Head }
      case _ => { _ == method }
    }
    matchingMethodsExcept(p)
  }

  private def matchingMethodsExcept(p: HttpMethod => Boolean) = {
    var methods = (methodRoutes filter { case (method, routes) =>
      !p(method) && (routes exists { _().isDefined })
    }).keys.toSet
    if (methods.contains(Get))
      methods += Head
    methods
  }

  def prependRoute(method: HttpMethod, route: UnfilteredRoute[T]): Unit =
    modifyRoutes(method, route +: _)

  def removeRoute(method: HttpMethod, route: UnfilteredRoute[T]): Unit =
    modifyRoutes(method, _ filterNot (_ == route))

  def beforeFilters: Seq[UnfilteredRoute[T]] = _beforeFilters

  def appendBeforeFilter(route: UnfilteredRoute[T]): Unit = _beforeFilters :+= route

  def afterFilters: Seq[UnfilteredRoute[T]] = _afterFilters

  def appendAfterFilter(route: UnfilteredRoute[T]): Unit = _afterFilters :+= route

  @tailrec private def modifyRoutes(method: HttpMethod, f: (Seq[UnfilteredRoute[T]] => Seq[UnfilteredRoute[T]])): Unit = {
    if (methodRoutes.putIfAbsent(method, f(Vector.empty)).isDefined) {
      val oldRoutes = methodRoutes(method)
      if (!methodRoutes.replace(method, oldRoutes, f(oldRoutes)))
       modifyRoutes(method, f)
    }
  }

  def entryPoints: Seq[String] =
    (for {
      (method, routes) <- methodRoutes
      route <- routes
    } yield method + " " + route).toSeq sortWith (_ < _)

  override def toString: String = entryPoints mkString ", "
}


trait ImplicitResponses {
  implicit def str2responseFn(str: String) =
    ResponseString(str)
  implicit def xml2html(xml: scala.xml.NodeSeq) =
    Html(xml)
}

trait Scalatra extends CoreDsl with ImplicitResponses {

  protected lazy val routes = new UnfilteredRouteRegistry[Any]()

  private lazy val _request = new DynamicVariable[HttpRequest[_]](null)
  private lazy val _multiParams = new DynamicVariable[MultiMap](null)
  def multiParams: MultiParams = _multiParams.value.withDefaultValue(Seq.empty)

  protected val _params = new MultiMapHeadView[String, String] with MapWithIndifferentAccess[String] {
    protected def multiMap = multiParams
  }

  def params = _params


  implicit def request = _request value

  protected implicit def string2RouteMatcher(path: String): RouteMatcher =
    new SinatraRouteMatcher(path, request.uri)

  protected implicit def pathPatternParser2RouteMatcher(pattern: PathPattern): RouteMatcher =
    new PathPatternRouteMatcher(pattern, request.uri)

  protected implicit def regex2RouteMatcher(regex: Regex): RouteMatcher =
    new RegexRouteMatcher(regex, request.uri)

  protected implicit def booleanBlock2RouteMatcher(block: => Boolean): RouteMatcher =
    new BooleanBlockRouteMatcher(block)

  protected case class HaltException(
      status: Option[Int],
      reason: Option[String],
      headers: Map[String, String],
      body: String)
   extends RuntimeException

  protected def executeRoutes:ResponseFunction[Any] =  {
    val result = try {
      runFilters(routes.beforeFilters)
      val actionResult = runRoutes(routes(HttpMethod(request.method))).headOption
      actionResult orElse matchOtherMethods() getOrElse doNotFound()
    }
    catch {
      case e: HaltException => {
        Status(e.status getOrElse 500) ~> ResponseString(e.body.toString)
      }
      case e => errorHandler(e)
    }
    finally {
      runFilters(routes.afterFilters)
    }
    result
  }

  protected def runFilters(filters: Traversable[UnfilteredRoute[Any]]) =
    for {
      route <- filters
      matchedRoute <- route()
    } invoke(matchedRoute)

  protected def runRoutes(routes: Traversable[UnfilteredRoute[Any]]) =
    for {
      route <- routes.toStream // toStream makes it lazy so we stop after match
      matchedRoute <- route()
      actionResult <- invoke(matchedRoute)
    } yield actionResult

  protected def invoke(matchedRoute: MatchedUnfilteredRoute[Any]) =
    withRouteMultiParams(Some(matchedRoute)) {
      try {
        Some(matchedRoute.action())
      }
      catch {
        case e: PassException => None
      }
    }

  protected def withRouteMultiParams[S](matchedRoute: Option[MatchedUnfilteredRoute[Any]])(thunk: => S): S = {
    val originalParams = multiParams
    _multiParams.withValue(originalParams ++ matchedRoute.map(_.multiParams).getOrElse(Map.empty))(thunk)
  }


  def pass() = throw new PassException

  protected class PassException extends RuntimeException

  def get(routeMatchers: RouteMatcher*)(action: => ResponseFunction[Any]) = addRoute(Get, routeMatchers, action)

  def post(routeMatchers: RouteMatcher*)(action: => ResponseFunction[Any]) = addRoute(Post, routeMatchers, action)

  def put(routeMatchers: RouteMatcher*)(action: => ResponseFunction[Any]) = addRoute(Put, routeMatchers, action)

  def delete(routeMatchers: RouteMatcher*)(action: => ResponseFunction[Any]) = addRoute(Delete, routeMatchers, action)

  def options(routeMatchers: RouteMatcher*)(action: => ResponseFunction[Any]) = addRoute(Options, routeMatchers, action)

  def patch(routeMatchers: RouteMatcher*)(action: => ResponseFunction[Any]) = addRoute(Patch, routeMatchers, action)

  protected def addRoute(method: HttpMethod, routeMatchers: Iterable[RouteMatcher], action: => ResponseFunction[Any]): UnfilteredRoute[Any] = {
    val route = UnfilteredRoute(routeMatchers, () => action, () => request.uri)
    routes.prependRoute(method, route)
    route
  }

  protected def removeRoute(method: HttpMethod, route: UnfilteredRoute[Any]): Unit =
    routes.removeRoute(method, route)

  protected def removeRoute(method: String, route: UnfilteredRoute[Any]): Unit =
    removeRoute(HttpMethod(method), route)


  def halt(status: JInteger = null,
           body: String,
           headers: Map[String, String] = Map.empty,
           reason: String = null): Nothing = {
    val statusOpt = if (status == null) None else Some(status.intValue)
    throw new HaltException(statusOpt, Some(reason), headers, body)
  }

  //capture all requests
  def intent: unfiltered.Cycle.Intent[Any,Any] = {
    case req @ _  =>  _request.withValue(req) {
      val multiMap = request.parameterNames.foldLeft(MultiMap()){ (acc, name) =>
        acc + (name -> request.parameterValues(name))
      }
      _multiParams.withValue(multiMap) {
        executeRoutes
      }
    }
  }

  private def matchOtherMethods(): Option[ResponseFunction[Any]] = {
    val allow = routes.matchingMethodsExcept(HttpMethod(request.method))
    if (allow.isEmpty) None else Some(doMethodNotAllowed(allow))
  }

  def before(routeMatchers: RouteMatcher*)(fun: => ResponseFunction[Any]) =
    addBefore(routeMatchers, fun)

  private def addBefore(routeMatchers: Iterable[RouteMatcher], fun: => ResponseFunction[Any]) =
    routes.appendBeforeFilter(UnfilteredRoute(routeMatchers, () => fun))

  def after(routeMatchers: RouteMatcher*)(fun: => ResponseFunction[Any]) =
    addAfter(routeMatchers, fun)

  private def addAfter(routeMatchers: Iterable[RouteMatcher], fun: => ResponseFunction[Any]) =
    routes.appendAfterFilter(UnfilteredRoute(routeMatchers, () => fun))

  protected var doMethodNotAllowed: (Set[HttpMethod] => ResponseFunction[Any]) = { allow =>
    MethodNotAllowed ~> ResponseString(allow mkString ", ")
  }
  def methodNotAllowed(f: Set[HttpMethod] => ResponseFunction[Any]) = doMethodNotAllowed = f

  protected var doNotFound: () => ResponseFunction[Any] = () => NotFound ~> ResponseString("could not find handler")
  def notFound(block: => ResponseFunction[Any]) {
    doNotFound = () => block
  }


  protected var errorHandler: UnfilteredErrorHandler = { case t => throw t }
  def error(handler: UnfilteredErrorHandler) = errorHandler = handler orElse errorHandler
}

class App extends Scalatra {

  get ("/html") {
    <html>
      <head></head>
      <body>Hello html</body>
    </html>
  }

  get ("/hello") {
     "hello world, hello request:"+request.toString
  }

  get ("/") {
     "hello index page!"
  }

  get("/param/:value") {
    "hello %s" format params('value)
  }
}


/** embedded server */
object Server {
  val logger = Logger(Server.getClass)
  def main(args: Array[String]) {
    val http = unfiltered.jetty.Http.anylocal // this will not be necessary in 0.4.0
    http.context("/assets") { _.resources(new java.net.URL(getClass().getResource("/www/css"), ".")) }
      .filter(new App with unfiltered.filter.Plan).run({ svr =>
        unfiltered.util.Browser.open(http.url)
      }, { svr =>
        logger.info("shutting down server")
      })
  }
}
