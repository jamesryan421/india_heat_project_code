"""Step 2 check (15 Oct 2026): does the Bryan-Morten earnings slope recover the flow elasticity when people also
differ in tastes for places?

Model. Person i chooses destination d to maximise eta*V_d + g_id, with g_id iid Gumbel(0,1), so choice shares are logit
with flow elasticity eta. The draw splits into a skill part and a taste part, g_id = lam*e_id + c_id, with e_id ~
Gumbel(0,1) and c_id = lam*log(S_id), S_id positive lam-stable (Laplace transform exp(-u**lam)); this c_id is Cardell's
(1997) C(lam) variable, so g_id stays exactly Gumbel. Log skill is log s_id = (lam/eta)*e_id and earnings are
log y_id = log w_d + log s_id, so skill is Frechet with shape eta/lam and lam = 1 is Bryan-Morten (all heterogeneity is
skill). Claim to check: the slope of movers' mean log earnings on log pi_d is about -lam**2/eta, so the earnings-based
theta = -1/slope is about eta/lam**2 = eta*(1 + var(taste)/var(skill)).
"""
import numpy as np

rng = np.random.default_rng(20261015)
EULER = 0.5772156649


def positive_stable(alpha, size):
    """Kanter's representation: E[exp(-u*S)] = exp(-u**alpha), 0 < alpha < 1."""
    u = rng.uniform(0.0, np.pi, size)
    w = rng.exponential(1.0, size)
    return (np.sin(alpha * u) / np.sin(u) ** (1.0 / alpha)) * (np.sin((1.0 - alpha) * u) / w) ** ((1.0 - alpha) / alpha)


def draws(lam, size):
    e = rng.gumbel(0.0, 1.0, size)
    c = np.zeros(size) if lam >= 1.0 else lam * np.log(positive_stable(lam, size))
    return e, lam * e + c


def simulate(eta, lam, n=400_000, J=40, chunk=50_000):
    v = np.linspace(-2.5, 2.5, J) / eta * 2.0          # destination values: shares from about 0.1% to 25%
    sum_e = np.zeros(J); cnt = np.zeros(J)
    for start in range(0, n, chunk):
        m = min(chunk, n - start)
        e, g = draws(lam, (m, J))
        k = np.argmax(eta * v + g, axis=1)
        np.add.at(sum_e, k, e[np.arange(m), k]); np.add.at(cnt, k, 1)
    ok = cnt >= 200
    pi = cnt[ok] / n
    mean_logs = (lam / eta) * sum_e[ok] / cnt[ok]
    slope_flow = np.polyfit(v[ok], np.log(pi), 1)[0]           # should equal eta
    slope_earn = np.polyfit(np.log(pi), mean_logs, 1)[0]       # claim: about -lam**2/eta
    lo, hi = np.log(pi) < np.median(np.log(pi)), np.log(pi) >= np.median(np.log(pi))
    s_lo = np.polyfit(np.log(pi)[lo], mean_logs[lo], 1)[0]
    s_hi = np.polyfit(np.log(pi)[hi], mean_logs[hi], 1)[0]
    return slope_flow, slope_earn, s_lo, s_hi, ok.sum(), pi.min(), pi.max()


if __name__ == "__main__":
    # 1. the taste-skill split keeps the composite draw Gumbel(0,1): mean = Euler's constant, variance = pi^2/6
    print("Check that g = lam*e + c is Gumbel(0,1): mean 0.5772, variance 1.6449, P(g <= 0) = exp(-1) = 0.3679")
    for lam in (1.0, 0.8, 0.6, 0.4):
        _, g = draws(lam, 2_000_000)
        print(f"  lam={lam:.1f}: mean {g.mean():.4f}  variance {g.var():.4f}  P(g<=0) {np.mean(g <= 0):.4f}")
    # 2. flow elasticity and earnings slope
    print("\neta   lam  var(taste)/var(skill)  flow slope (=eta?)  earnings theta = -1/slope  eta/lam^2   "
          "theta from low-share half / high-share half   destinations, share range")
    for eta in (3.0, 6.0):
        for lam in (1.0, 0.8, 0.6, 0.4):
            sf, se_, slo, shi, J, pmin, pmax = simulate(eta, lam)
            print(f"{eta:4.1f}  {lam:.1f}  {1/lam**2 - 1:8.2f}            {sf:7.3f}            {-1/se_:8.2f}             "
                  f"{eta/lam**2:7.2f}      {-1/slo:8.2f} / {-1/shi:8.2f}                  {J}, {pmin:.4f}-{pmax:.3f}")
