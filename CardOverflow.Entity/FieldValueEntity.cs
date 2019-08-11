using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace CardOverflow.Entity
{
    public partial class FieldValueEntity
    {
        public int FacetInstanceId { get; set; }
        public int FieldId { get; set; }
        [Required]
        public string Value { get; set; }

        [ForeignKey("FacetInstanceId")]
        [InverseProperty("FieldValues")]
        public virtual FacetInstanceEntity FacetInstance { get; set; }
        [ForeignKey("FieldId")]
        [InverseProperty("FieldValues")]
        public virtual FieldEntity Field { get; set; }
    }
}
