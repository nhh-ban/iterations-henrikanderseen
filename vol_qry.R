#Making GQL for volumes

vol_qry <- function(id, from, to) {
  query <- glue::glue(
    "{
        trafficData(trafficRegistrationPointId: \"[[id]]\") {
          volume {
            byHour(from: \"[[from]]\", to: \"[[to]]\") {
              edges {
                node {
                  from
                    to
                      total {
                        volumeNumbers {
                          volume
                        }
                      }
                    }
                  }
                }
              }
            }
          }",
    .open = "[[", 
    .close = "]]"
  )
  return(query)
}


vol_qry("97411V72313", "2022-05-01T06:55:47Z", "2022-05-08T06:55:47Z")