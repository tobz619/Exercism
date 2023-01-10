def to_rna(dna_strand):
    rna = ""
    for nucleotide in dna_strand:
        rna += (complement(nucleotide.upper()))
    return rna


def complement(rna):
    return rna_dict[rna]

rna_dict = { 'A' : 'U',
             'G' : 'C',
             'C' : 'G',
             'T' : 'A'    
           }