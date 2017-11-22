mhs <- 0:100
ghs <- 0:100

k <- sapply(mhs, function(mh) {
  mh <- 1
  sapply(ghs, function(gh) {
    m_side <- data.table(m = c(rep(TRUE, mh), rep(FALSE, 100 - mh)),
                         merger = TRUE)[sample(.N)]
    g_side <- data.table(g = c(rep(TRUE, gh), rep(FALSE, 100 - gh)),
                         merger = TRUE)[sample(.N)]
    mg <- cbind(m_side, g_side)
    #mg <- merge(m_side, g_side, by = 'merger', allow.cartesian = TRUE)
    info_gain(mg$m, mg$g)
  })
})
