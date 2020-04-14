using System;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata;
using Microsoft.AspNetCore.Identity;
using Microsoft.AspNetCore.Identity.EntityFrameworkCore;
using System.Security.Cryptography;
using System.Threading.Tasks;
using System.Threading;
using System.Linq;
using Microsoft.FSharp.Core;
using System.Collections.Generic;
using Microsoft.EntityFrameworkCore.Infrastructure;
using Microsoft.Extensions.DependencyInjection;
using System.Collections;

namespace CardOverflow.Entity {

  public interface IEntityHasher {
    FSharpFunc<(CardInstanceEntity, BitArray, SHA512), BitArray> CardInstanceHasher { get; }
    FSharpFunc<(TemplateInstanceEntity, SHA512), BitArray> TemplateInstanceHasher { get; }
  }

  // This class should not store custom state due to usage of `AddDbContextPool`
  public partial class CardOverflowDb : IdentityDbContext<UserEntity, IdentityRole<int>, int> {
    private readonly IEntityHasher _entityHasher;

    public CardOverflowDb(DbContextOptions<CardOverflowDb> options) : base(options) {
      _entityHasher = this.GetService<IEntityHasher>(); // lowTODO consider injecting the SHA512 hasher; it's also IDisposable
    }

    public override int SaveChanges(bool acceptAllChangesOnSuccess) {
      _OnBeforeSaving();
      return base.SaveChanges(acceptAllChangesOnSuccess);
    }

    public override Task<int> SaveChangesAsync(bool acceptAllChangesOnSuccess, CancellationToken cancellationToken = default) {
      _OnBeforeSaving();
      return base.SaveChangesAsync(acceptAllChangesOnSuccess, cancellationToken);
    }

    private void _OnBeforeSaving() {
      var entries = ChangeTracker.Entries().ToList();
      using var sha512 = SHA512.Create();
      foreach (var x in entries.Where(x => x.Entity is TemplateInstanceEntity)) {
        var template = (TemplateInstanceEntity) x.Entity;
        template.Hash = _entityHasher.TemplateInstanceHasher.Invoke((template, sha512));
      }
      foreach (var x in entries.Where(x => x.Entity is CardInstanceEntity)) {
        var card = (CardInstanceEntity) x.Entity;
        var templateHash = card.TemplateInstance?.Hash ?? TemplateInstance.Find(card.TemplateInstanceId).Hash;
        card.Hash = _entityHasher.CardInstanceHasher.Invoke((card, templateHash, sha512));
      }
    }

    public IQueryable<AcquiredCardIsLatestEntity> AcquiredCardIsLatest => _AcquiredCardIsLatestTracked.AsNoTracking();
    public IQueryable<CardInstanceRelationshipCountEntity> CardInstanceRelationshipCount => _CardInstanceRelationshipCountTracked.AsNoTracking();
    public IQueryable<CardInstanceTagCountEntity> CardInstanceTagCount => _CardInstanceTagCountTracked.AsNoTracking();
    public IQueryable<CardRelationshipCountEntity> CardRelationshipCount => _CardRelationshipCountTracked.AsNoTracking();
    public IQueryable<CardTagCountEntity> CardTagCount => _CardTagCountTracked.AsNoTracking();
    public IQueryable<LatestCardInstanceEntity> LatestCardInstance => _LatestCardInstanceTracked.AsNoTracking();
    public IQueryable<LatestCommunalFieldInstanceEntity> LatestCommunalFieldInstance => _LatestCommunalFieldInstanceTracked.AsNoTracking();
    public IQueryable<LatestTemplateInstanceEntity> LatestTemplateInstance => _LatestTemplateInstanceTracked.AsNoTracking();

  }
}
