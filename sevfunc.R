SEV <- function(h0 = 0, h1 = "greater", sig = 0.05, sd = 2, n = 100,
                outcome = 0.4, range = c(0, 1), interval = 0.01,
                distfunc = pnorm) {

  # Do sig test.
  .se <- sd/sqrt(n)
  .z <- (outcome-h0)/.se
  .p <- 1-distfunc(.z)
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
  .sev <- 1-distfunc(.zs)

  .r <- list(se = .se, z = .z, p = .p, sig = .sig, x = .x, ds = .ds, zs = .zs,
             sev = .sev)
}

# Some configurability by passing custom distribution function.
tdist <- function(q) { pt(q = q, df = n - 1) }

# Get severity for t Test and H0: mu = 1; H1: mu > 1.
sev <- SEV(h0 = 0, distfunc = tdist, n = 50)

# print(sev)
plot(unlist(sev["sev"]), type = "l")

