box::use(DBI[dbConnect, dbGetQuery, dbDisconnect],
         RMySQL[MySQL],
         ssh[ssh_connect, ssh_disconnect],
         shiny[navbarPage, reactiveValues, NS, moduleServer, observeEvent, showTab, hideTab, uiOutput, renderUI, tagList, h2, h3, navbarMenu, insertTab, tabPanel, div, appendTab],
         shinyjs[useShinyjs, hidden, hide, show],
         tibble[tibble],
         dplyr[tbl, collect, `%>%`])

box::use(
  app / view / login,
  app / view / manual,
  app / view / batch,
  app / view / locations,
  app / view / locUpload,
)

box::use(app / logic / myNotification[myNotification],
         app / logic / getStorageLocs[getStorageLocs],)

#' @export
ui <- function(id) {
  ns <- NS(id)
  useShinyjs()
  navbarPage(
    title = "Collective Access Admin App",
    id = ns("page"),
    useShinyjs(),
    login$ui(ns('login')),
    tabPanel(
      title = "Home",
      value = "home",
      h2("Welcome to the Collective Access Admin App"),
      div(id = ns('loginMessage'), h3("Please login to continue")),
      navbarPage(title = "Admin Options",
                 id = ns("nav")#,
                 # navbarMenu(title = "Locations",
                 #            menuName = ns("locationMenu"))
      )
    ),

  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- NS(id)

    # input <<- input

    con = dbConnect(
      MySQL(),
      host = "10.126.24.122",
      dbname = "ca",
      user = Sys.getenv("uid"),
      password = Sys.getenv("pwd"),
      port = 3306
    )

    con2 = dbConnect(
      MySQL(),
      host = "10.126.24.122",
      dbname = "shiny",
      user = Sys.getenv("uid"),
      password = Sys.getenv("pwd"),
      port = 3306
    )

    dir = "/mnt/storage/public"

    sshSession = ssh_connect("ca@10.126.24.122", passwd = Sys.getenv("pwd"))

    database = tbl(con2, "user_data") %>%
      collect()

    rvals = reactiveValues(df = tibble(),
                           storageLocations = getStorageLocs(con))

    credentials = login$server('login', id, database, con2)
    manual$server('manual', id, sshSession, credentials, rvals, dir)
    batch$server('batch', sshSession, rvals, dir)
    locations$server("locations", rvals)
    locUpload$server("locUpload", sshSession, rvals, con, dir)

    observeEvent(credentials()$user_auth, {
      if (credentials()$user_auth) {
        print("creating tabs")
        appendTab(
          inputId = "nav",
          navbarMenu(
            title = "Locations",
            manual$ui(ns('manual')),
            batch$ui(ns('batch')),
            locations$ui(ns('locations')),
            locUpload$ui(ns('locUpload'))
          )
        )
        hide("loginMessage")
      } else {
        print("hiding tabs")
        hideTab("nav", "manualentry")
        hideTab("nav", "newstoragelocations")
        hideTab("nav", "storageLocations")
        hideTab("nav", "batchimport")
        show("loginMessage")
      }
    })

    session$onSessionEnded(function() {
      dbDisconnect(con)
      dbDisconnect(con2)
      ssh_disconnect(sshSession)
    })

  })
}
