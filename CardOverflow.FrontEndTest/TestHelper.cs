using Blazored.Toast;
using BlazorStrap;
using Bunit;
using CardOverflow.Debug;
using CardOverflow.Entity;
using CardOverflow.Server;
using CardOverflow.Server.Pages.Deck;
using FluentValidation;
using FsToolkit.ErrorHandling;
using Microsoft.AspNetCore.Components;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Storage;
using Microsoft.Extensions.DependencyInjection;
using System;
using System.Threading.Tasks;

namespace CardOverflow.FrontEndTest {
  public static class TestHelper {
    
    public static T SideEffect<T>(T input, Action<T> action) {
      action(input);
      return input;
    }

    public static bool Not(bool input) => !input;

    public static void Setup(TestServiceProvider services, Action<CardOverflowDb> setupDb) {
      if (!services.IsProviderInitialized) {
        services.AddSingleton<IEntityHasher, ContainerExtensions.EntityHasher>();
        services.AddBlazoredToast();
        services.AddScoped<NavigationManager, MockNavigationManager>();
        services.AddBootstrapCss();
        services.AddTransient<IValidator<FollowCommandViewModel>, FollowCommandViewModelValidator>();
        services.AddSingleton<DbExecutor>();
        services.AddDbContextPool<CardOverflowDb>(x => x.UseInMemoryDatabase(Guid.NewGuid().ToString()));
      }

      using var databaseContext = services.GetService<CardOverflowDb>();
      databaseContext.Database.EnsureDeleted();
      databaseContext.Database.EnsureCreated();
      setupDb(databaseContext);
      databaseContext.SaveChanges();
      foreach (var entity in databaseContext.ChangeTracker.Entries()) {
        entity.State = EntityState.Detached;
      }
    }

  }
}