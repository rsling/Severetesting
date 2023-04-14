SEV <- function(h0 = 0, h1 = "greater", sig = 0.05, sd = 2, n = 100,
                outcome = 0.4, range = c(0, 1), interval = 0.01,
                distr = pnorm) {

  # Do sig test.
  .se <- sd/sqrt(n)
  .z <- (outcome-h0)/.se
  .p <- 1-distr(.z)
  .sig <- .p <= sig
  if (!.sig) {
    warning(paste0("p = ", round(.p,4), " < ", round(sig, 4),
                   "! Do not interpret severity."))
    }

  # Get hypothetical discrepancies.
  .ds <- seq(range[1], range[2], interval)
  .x <- seq(1, length(.ds), 1)

  # Get SEV for hypothetical discrepancies.
  .zs <- .ds/.se
  .zs <- .zs - .z
  .sevs <- 1-distr(.zs)

  # Return everything.
  list(se = .se, z = .z, p = .p, sig = .sig, x = .x, ds = .ds, zs = .zs,
             sevs = .sevs, p.h0 = h0, p.h1 = h1, p.sig = sig, p.sd = sd,
             p.n = n, p.outcome = outcome)
}


# Pass the result of SEV() and a discrepancy that you want the SEV value for.
SEV.query <- function(mu, sev, distr = pnorm) {
  if (!sev[["sig"]]) {
    warning(paste0("p = ", round(sev[["p"]],4), " < ", round(sev[["p.sig"]],4),
                   "! Do not interpret severity."))
  }
  .z <- (mu-sev[["p.outcome"]])/sev[["se"]]
  1-distr(.z)
}

# Some configurability by passing custom distribution function.
# Get severity for t Test and H0: mu = 1; H1: mu > 1.
# tdist <- function(q) { pt(q = q, df = n - 1) }
# sev <- SEV(distr = tdist)

sev <- SEV()
plot(sev[["sevs"]], type = "l")
print(SEV.query(mu = 0.2, sev = sev))
