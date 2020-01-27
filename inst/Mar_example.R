test1 <- makeDATRAS(region = "Mar", yr = 2017:2018, season = c("SPRING","SUMMER"),
                fn.oracle.username = oracle.username,
                fn.oracle.password = oracle.password,
                fn.oracle.dsn = oracle.dsn,
                data.dir = data.dir,
                usepkg = "roracle")
