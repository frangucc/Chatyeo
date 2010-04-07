{application, chatyeo_email,
 [{description, "Simple email sending application using gmail"},
  {vsn, "1.0"},
  {modules, [chatyeo_email, chatyeo_email_sup, new_smtp, chatyeo_email_app]},
  {registered, [chatyeo_email, chatyeo_email_sup]},
  {applications, [kernel, stdlib, ssl]},
  {mod, {chatyeo_email_app,[]}}]}.
