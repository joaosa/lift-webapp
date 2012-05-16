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
import code.service.{Test, Service}

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
      User, Subscription, Device, Message, Notification,
      Data, Point)

    // where to search snippet
    LiftRules.addToPackages("code")

    // Build SiteMap
    def sitemap() = SiteMap(
      Menu(S ? "Home") / "index",
      Menu(S ? "User") / "login" submenus (),
      Menu(S ? "Admins") / "admins" / ** submenus (
        Menu(S ? "Notify") / "notify",
        Menu(S ? "Users") / "users" / "index" submenus (User.menus ::: Subscription.menus ::: Device.menus),
        Menu(S ? "Messages") / "messages" / "index" submenus (Message.menus),
        Menu(S ? "Notifications") / "notifications" / "index" submenus (Notification.menus),
        Menu(S ? "Data") / "data" / "index" submenus (Data.menus ::: Point.menus)))

    //def sitemapMutators = User.sitemapMutator
    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    //LiftRules.setSiteMapFunc(() => sitemapMutators(sitemap))
    LiftRules.setSiteMapFunc(() => sitemap())

    // webservice authentication
    val withAuthentication: PartialFunction[Req, Unit] = {
      case _ if Service.LoggedIn.is =>
    }

    // Hook RestHelper to boot
    // stateful -- associated with a servlet container session
    LiftRules.dispatch.append(Service)
    LiftRules.dispatch.append(withAuthentication guard Test)
    LiftRules.dispatch.append(withAuthentication guard User)
    // stateless -- no session created
    //LiftRules.statelessDispatch.append(User)
    LiftRules.statelessDispatch.append(Subscription)
    LiftRules.statelessDispatch.append(Device)
    LiftRules.statelessDispatch.append(Message)
    LiftRules.statelessDispatch.append(Notification)
    LiftRules.statelessDispatch.append(Data)
    LiftRules.statelessDispatch.append(Point)

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

    // Authentication
    // This resource is protected by an AuthRole named admin.
    LiftRules.httpAuthProtectedResource.prepend {
      case Req("secure-basic" :: Nil, _, _) => Full(AuthRole("admin"))
    }

    LiftRules.authentication = HttpBasicAuthentication("Authenticate yourself") {
      case (login, password, req) => User.login(login, password)
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
