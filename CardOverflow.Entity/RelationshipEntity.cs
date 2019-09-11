using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace CardOverflow.Entity
{
    public partial class RelationshipEntity
    {
        public int Id { get; set; }
        public int SourceId { get; set; }
        public int TargetId { get; set; }
        [Required]
        [Column("Relationship")]
        [StringLength(250)]
        public string Name {
            get => _Relationship1;
            set {
                if (value.Length > 250) throw new ArgumentOutOfRangeException($"String too long! It was {value.Length} long, and Relationship1 has a maximum length of 250. Attempted value: {value}");
                _Relationship1 = value;
            }
        }
        private string _Relationship1;

        [ForeignKey("SourceId")]
        [InverseProperty("RelationshipSources")]
        public virtual CardEntity Source { get; set; }
        [ForeignKey("TargetId")]
        [InverseProperty("RelationshipTargets")]
        public virtual CardEntity Target { get; set; }
    }
}
