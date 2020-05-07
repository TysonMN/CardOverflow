using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Collections;
using NpgsqlTypes;

namespace CardOverflow.Entity
{
    public partial class BranchInstanceEntity
    {
        public BranchInstanceEntity()
        {
            AcquiredCards = new HashSet<AcquiredCardEntity>();
            CardCopySources = new HashSet<CardEntity>();
            CardLatestInstances = new HashSet<CardEntity>();
            CommunalFieldInstance_CardInstances = new HashSet<CommunalFieldInstance_CardInstanceEntity>();
            File_CardInstances = new HashSet<File_CardInstanceEntity>();
            CardTagCounts = new HashSet<CardTagCountEntity>();
            CardRelationshipCounts = new HashSet<CardRelationshipCountEntity>();
            CardInstanceTagCounts = new HashSet<CardInstanceTagCountEntity>();
            CardInstanceRelationshipCounts = new HashSet<CardInstanceRelationshipCountEntity>();
        }

        [Key]
        public int Id { get; set; }
        public DateTime Created { get; set; }
        public DateTime? Modified { get; set; }
        public int BranchId { get; set; }
        public bool IsDmca { get; set; }
        [Required]
        public string FieldValues { get; set; }
        public int TemplateInstanceId { get; set; }
        public int Users { get; set; }
        [Required]
        [StringLength(200)]
        public string EditSummary {
            get => _EditSummary;
            set {
                if (value.Length > 200) throw new ArgumentOutOfRangeException($"String too long! It was {value.Length} long, and EditSummary has a maximum length of 200. Attempted value: {value}");
                _EditSummary = value;
            }
        }
        private string _EditSummary;
        public long? AnkiNoteId { get; set; }
        public short? AnkiNoteOrd { get; set; }
        [Required]
        [Column(TypeName = "bit(512)")]
        public BitArray Hash { get; set; }
        public string TsVectorHelper { get; set; }
        public NpgsqlTsVector TsVector { get; set; }

        [ForeignKey("BranchId")]
        [InverseProperty("BranchInstances")]
        public virtual BranchEntity Branch { get; set; }
        [ForeignKey("TemplateInstanceId")]
        [InverseProperty("BranchInstances")]
        public virtual TemplateInstanceEntity TemplateInstance { get; set; }
        [InverseProperty("BranchInstance")]
        public virtual ICollection<AcquiredCardEntity> AcquiredCards { get; set; }
        [InverseProperty("CopySource")]
        public virtual ICollection<CardEntity> CardCopySources { get; set; }
        [InverseProperty("LatestInstance")]
        public virtual ICollection<CardEntity> CardLatestInstances { get; set; }
        [InverseProperty("CardInstance")]
        public virtual ICollection<CommunalFieldInstance_CardInstanceEntity> CommunalFieldInstance_CardInstances { get; set; }
        [InverseProperty("CardInstance")]
        public virtual ICollection<File_CardInstanceEntity> File_CardInstances { get; set; }

        public virtual ICollection<CardTagCountEntity> CardTagCounts { get; set; }
        public virtual ICollection<CardRelationshipCountEntity> CardRelationshipCounts { get; set; }
        public virtual ICollection<BranchInstanceTagCountEntity> BranchInstanceTagCounts { get; set; }
        public virtual ICollection<BranchInstanceRelationshipCountEntity> BranchInstanceRelationshipCounts { get; set; }
    }
}
