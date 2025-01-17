﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Authentication.Cookies;
using Microsoft.AspNetCore.Authentication.OpenIdConnect;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.RazorPages;

namespace CardOverflow.Server.Pages {
  public class LogoutModel : PageModel {
    public async Task OnGetAsync() {
      await HttpContext.SignOutAsync(CookieAuthenticationDefaults.AuthenticationScheme);
      await HttpContext.SignOutAsync(OpenIdConnectDefaults.AuthenticationScheme);
    }
  }
}
