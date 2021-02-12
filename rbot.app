{application, rbot,
 [{description, "An OTP application"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, {rbot, []}},
  {applications,
   [kernel,
    stdlib,
    crypto,
    asn1,
    public_key,
    ssl,
    inets
   ]},
  {env,[]},
  {modules, []},

  {licenses, ["MPL-2.0"]},
  {links, []}
 ]}.

