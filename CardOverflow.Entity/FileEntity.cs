﻿using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace CardOverflow.Entity
{
[Table("File")]
    public partial class FileEntity
    {
        public int Id { get; set; }
        public int UserId { get; set; }
        public byte MediaType { get; set; }
        [Required]
        [StringLength(100)]
        public string FileName { get; set; }
        [Required]
        public byte[] Data { get; set; }

        [ForeignKey("UserId")]
        [InverseProperty("Files")]
        public virtual UserEntity User { get; set; }
    }
}
