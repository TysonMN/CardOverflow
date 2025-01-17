using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Threading.Tasks;
using CardOverflow.Pure;
using Microsoft.AspNetCore.Identity;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.RazorPages;
using Microsoft.EntityFrameworkCore;
using ThoughtDesign.IdentityProvider.Areas.Identity.Data;
using ThoughtDesign.WebLibrary;

namespace ThoughtDesign.IdentityProvider.Areas.Identity.Pages.Account.Manage {
  public partial class IndexModel : PageModel {
    private readonly UserManager<ThoughtDesignUser> _userManager;
    private readonly SignInManager<ThoughtDesignUser> _signInManager;
    private readonly DbExecutor _db;

    public IndexModel(
      UserManager<ThoughtDesignUser> userManager,
      SignInManager<ThoughtDesignUser> signInManager,
      DbExecutor db) {
      _userManager = userManager;
      _signInManager = signInManager;
      _db = db;
    }

    public string Username { get; set; }

    [TempData]
    public string StatusMessage { get; set; }

    [BindProperty]
    public InputModel Input { get; set; }

    public class InputModel {
      private string _displayName;

      [Required] // these attributes should mirror RegisterModel's InputModel's DisplayName
      [StringLength(32, ErrorMessage = "The {0} must be at least {2} and at max {1} characters long.", MinimumLength = 3)]
      [Display(Name = "Display Name")]
      public string DisplayName {
        get => _displayName;
        set => _displayName = value.Trim().Pipe(MappingTools.standardizeWhitespace);
      }
    }

    private void Load(ThoughtDesignUser user) {
      Username = user.UserName;
      Input = new InputModel {
        DisplayName = user.DisplayName
      };
    }

    public async Task<IActionResult> OnGetAsync() {
      var user = await _userManager.GetUserAsync(User);
      if (user == null) {
        return NotFound($"Unable to load user with ID '{_userManager.GetUserId(User)}'.");
      }

      Load(user);
      return Page();
    }

    public async Task<IActionResult> OnPostAsync() {
      var user = await _userManager.GetUserAsync(User);
      if (user == null) {
        return NotFound($"Unable to load user with ID '{_userManager.GetUserId(User)}'.");
      }

      if (!ModelState.IsValid) {
        Load(user);
        return Page();
      }

      if (Input.DisplayName != user.DisplayName) {

        user.DisplayName = Input.DisplayName;
        var setDisplayNameResult = await _userManager.UpdateAsync(user);
        if (!setDisplayNameResult.Succeeded) {
          StatusMessage = setDisplayNameResult.Errors.Select(x => $"{x.Code} - {x.Description}.").Pipe(x => string.Join("\r\n", x)).Pipe(x => $"An error occured while updating the Display Name: \r\n{x}");
          return RedirectToPage();
        } else {
          try {
            await _db.CommandAsync(async db => {
              var userEntity = await db.User.SingleAsync(x => x.Id == user.Id);
              userEntity.DisplayName = Input.DisplayName;
              await db.SaveChangesAsync();
            });
          } catch {
            StatusMessage = "An error occured while updating the Display Name. Please try again or contact support."; // lowTODO add logging
            return RedirectToPage();
          }
        }
      }

      await _signInManager.RefreshSignInAsync(user);
      StatusMessage = "Your profile has been updated";
      return RedirectToPage();
    }
  }
}
