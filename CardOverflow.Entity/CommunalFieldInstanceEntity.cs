using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using NpgsqlTypes;

namespace CardOverflow.Entity
{
    public partial class CommunalFieldInstanceEntity
    {
        public CommunalFieldInstanceEntity()
        {
            CommunalFieldInstance_BranchInstances = new HashSet<CommunalFieldInstance_BranchInstanceEntity>();
            CommunalFields = new HashSet<CommunalFieldEntity>();
        }

        [Key]
        public int Id { get; set; }
        public int CommunalFieldId { get; set; }
        [Required]
        [StringLength(200)]
        public string FieldName {
            get => _FieldName;
            set {
                if (value.Length > 200) throw new ArgumentOutOfRangeException($"String too long! It was {value.Length} long, and FieldName has a maximum length of 200. Attempted value: {value}");
                _FieldName = value;
            }
        }
        private string _FieldName;
        [Required]
        public string Value { get; set; }
        public DateTime Created { get; set; }
        public DateTime? Modified { get; set; }
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
        public string BWeightTsVectorHelper { get; set; }
        public NpgsqlTsVector TsVector { get; set; }

        [ForeignKey("CommunalFieldId")]
        [InverseProperty("CommunalFieldInstances")]
        public virtual CommunalFieldEntity CommunalField { get; set; }
        [InverseProperty("CommunalFieldInstance")]
        public virtual ICollection<CommunalFieldInstance_BranchInstanceEntity> CommunalFieldInstance_BranchInstances { get; set; }
        [InverseProperty("LatestInstance")]
        public virtual ICollection<CommunalFieldEntity> CommunalFields { get; set; }
    }
}
