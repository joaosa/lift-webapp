package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._
import common._
import http._
import sitemap._
import mapper._
import code.model._
import net.liftweb.widgets.flot._
import reactive.web.Reactions
import net.liftweb.http.ResourceServer
import net.liftweb.http.auth.HttpBasicAuthentication
import net.liftweb.http.auth.AuthRole
import akka.actor.ActorSystem
import code.service.Service
import code.service.Service.withAuthentication

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot extends Loggable {
  def boot() {
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor =
        new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
          Props.get("db.url") openOr
            "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
          Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }
    
    // Use Lift's Mapper ORM to populate the database
    // you don't need to use Mapper to use Lift... use
    // any ORM you want
    Schemifier.schemify(true, Schemifier.infoF _,
      Location, User, Subscription, Device, Message, Notification,
      Data, Point)

    // where to search snippet
    LiftRules.addToPackages("code")

    // Build SiteMap
    def sitemap() = SiteMap(
      Menu(S ? "Home") / "index",
      Menu(S ? "User") / "users" / "index" submenus (
        Menu(S ? "Login") / "users" / "login"),
      Menu(S ? "Admins") / "admins" / "index" submenus (
        Menu(S ? "Location") / "locations" / "admin" submenus (
          Location.menus),
        Menu(S ? "Notify") / "notify",
        Menu(S ? "Users") / "users" / "admin" submenus (User.menus :::
          Subscription.menus ::: Device.menus),
        Menu(S ? "Messages") / "messages" / "admin" submenus (Message.menus),
        Menu(S ? "Notifications") / "notifications" / "admin" submenus (
          Notification.menus),
        Menu(S ? "Data") / "data" / "admin" submenus (
          Data.menus ::: Point.menus)))

    //def sitemapMutators = User.sitemapMutator
    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    //LiftRules.setSiteMapFunc(() => sitemapMutators(sitemap))
    LiftRules.setSiteMapFunc(() => sitemap())

    def requireSSL() = {
      for {
        r <- S.request
        lowLevelReq <- Box !! r if lowLevelReq.request.scheme == "http"
      } yield {
        S.redirectTo("https://" + lowLevelReq.request.serverName + lowLevelReq.contextPath)
      }
      Empty
    }

    //object RequireSSL extends Loc.EarlyResponse(requireSSL)

    // Hook RestHelper to boot
    // stateful -- associated with a servlet container session
    LiftRules.dispatch.append(Service)
    LiftRules.dispatch.append(withAuthentication guard Location)
    LiftRules.dispatch.append(withAuthentication guard User)
    LiftRules.dispatch.append(withAuthentication guard Subscription)
    LiftRules.dispatch.append(withAuthentication guard Device)
    LiftRules.dispatch.append(withAuthentication guard Message)
    LiftRules.dispatch.append(withAuthentication guard Notification)
    LiftRules.dispatch.append(withAuthentication guard Data)
    LiftRules.dispatch.append(withAuthentication guard Point)
    // stateless -- no session created
    //LiftRules.statelessDispatch.append(User)

    // Use jQuery 1.4
    LiftRules.jsArtifacts = net.liftweb.http.js.jquery.JQuery14Artifacts

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Make notices fade out
    LiftRules.noticesAutoFadeOut.default.set((noticeType: NoticeType.Value) =>
      Full((1.seconds, 2.seconds)))

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // What is the function to test if a user is logged in?
    //LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))

    // Make a transaction span the whole HTTP request
    S.addAround(DB.buildLoanWrapper())

    // Authorization
    val roles = AuthRole("admin", AuthRole("user"))

    // Authentication
    // This resource is protected by an AuthRole named admin.
    LiftRules.httpAuthProtectedResource.prepend {
      case Req("users" :: xs, _, _) => roles.getRoleByName("user")
      case Req("admins" :: xs, _, _) => roles.getRoleByName("admin")
    }

    LiftRules.authentication = HttpBasicAuthentication("Authenticate yourself") {
      case (login, password, req) => Service.login(login, password)
    }

    // Start reactive web
    Reactions.init(true)

    // Start the Flot Javascript plotting library
    Flot.init()

    // Load Flot plugins
    ResourceServer.allow({
      case "flot" :: "jquery.flot.text.js" :: Nil => true
    })

    // Remote User Actor System
    ActorSystem("userActorSystem")
  }
}
