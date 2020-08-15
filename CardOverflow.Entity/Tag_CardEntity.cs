﻿using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace CardOverflow.Entity
{
    [Table("tag_2_card")]
    public partial class Tag_CardEntity
    {
        [Key]
        public int TagId { get; set; }
        [Key]
        public int UserId { get; set; }
        [Key]
        public int StackId { get; set; }
        public int CardId { get; set; }
        [DatabaseGenerated(DatabaseGeneratedOption.Identity)]
        public DateTime Created { get; set; }

        [ForeignKey("CardId")]
        [InverseProperty("Tag_Cards")]
        public virtual CardEntity Card { get; set; }
        [ForeignKey("TagId")]
        [InverseProperty("Tag_Cards")]
        public virtual TagEntity Tag { get; set; }
        [ForeignKey("StackId")]
        public virtual StackEntity Stack { get; set; }
    }
}
