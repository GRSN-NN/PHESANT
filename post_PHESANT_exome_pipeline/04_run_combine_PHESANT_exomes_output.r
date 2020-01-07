rm(list=ls())

library(data.table)
library(dplyr)

only_males_file <- "../should_only_be_in_males.tsv"
only_females_file <- "../should_only_be_in_females.tsv"

n_chunks <- 10
n_exomes <- '200k'
date <- 'November_2019'

QCed_io_name <- '../../pharma_parsed_and_restricted_to_200K_sample_subset'
final_output <- '../../phesant_output_'

# If the file doesn't exists, then in that chunk, no cat phenotypes made it through PHESANT.
pheno_file <- paste0(QCed_io_name, "_cat_variables_both_sexes.1.tsv")

if(file.exists(pheno_file))
	pheno <- fread(pheno_file, sep='\t', data.table=FALSE, header=TRUE)

if(n_chunks > 1) {
	for(i in 2:n_chunks)
	{	
		pheno_file <- paste0(QCed_io_name, "_cat_variables_both_sexes.", i, ".tsv")
		
		if(!file.exists(pheno_file))
			next
		
		pheno_tmp <- fread(pheno_file, sep='\t', data.table=FALSE, header=TRUE)
		# check 
		print(all(pheno_tmp[,1] == pheno[,1]))
		pheno <- cbind(pheno, pheno_tmp[,-1, drop=FALSE])
	}
}

# Remove sex specific 
males_only <- fread(only_males_file, sep='\t', data.table=FALSE)$V1
females_only <- fread(only_females_file, sep='\t', data.table=FALSE)$V1

if(exists("pheno"))
	pheno <- pheno[,-which(colnames(pheno) %in% c(males_only, females_only))]

# Next the cts raw
if(file.exists(paste0(QCed_io_name, "_cts_raw.tsv")))
{
	pheno_tmp <- fread(paste0(QCed_io_name, "_cts_raw.tsv"), sep='\t', data.table=FALSE, header=TRUE)
	
	if (any(colnames(pheno_tmp) %in% c(males_only, females_only)))
		pheno_tmp <- pheno_tmp[,-which(colnames(pheno_tmp) %in% c(males_only, females_only))]

	if(!exists("pheno")) {
		names(pheno_tmp) <- paste0(names(pheno_tmp), '_raw')
		pheno <- pheno_tmp
	} else {
		print(all(order(pheno_tmp[,1] == pheno[,1])))
		names(pheno_tmp) <- paste0(names(pheno_tmp), '_raw')
		pheno <- cbind(pheno, pheno_tmp[,-1, drop=FALSE])
	}
}

# Finally, cts IRNT
if(!exists("pheno")) 
	print("no phenotype output files exist!")

if(file.exists(paste0(QCed_io_name, "_cts_irnt.tsv")))
{
	pheno_tmp <- fread(paste0(QCed_io_name, "_cts_irnt.tsv"), sep='\t', data.table=FALSE, header=TRUE)

	if (any(colnames(pheno_tmp) %in% c(males_only, females_only)))
		pheno_tmp <- pheno_tmp[,-which(colnames(pheno_tmp) %in% c(males_only, females_only))]

	if(!exists("pheno")) {
		names(pheno_tmp) <- paste0(names(pheno_tmp), '_irnt')
		pheno <- pheno_tmp
	} else {
		print(all(order(pheno_tmp[,1]) == order(pheno[,1])))
		names(pheno_tmp) <- paste0(names(pheno_tmp), '_irnt')
		pheno <- cbind(pheno, pheno_tmp[,-1, drop=FALSE])
	}
}

colnames(pheno)[1] <- 'userId'

# Combine the phenotype summary files (these have already been restricted to sex-specific).
pheno_summary_file <- paste0(QCed_io_name, "_cat_variables_both_sexes_phesant_recodings_remove_sex_specific.1_phenosummary.tsv")
if(file.exists(pheno_summary_file))
	pheno_summary <- read.table(pheno_summary_file, sep='\t', quote="", comment.char="", header=TRUE, stringsAsFactors=FALSE)

if(n_chunks > 1) {
	for(i in 2:n_chunks)
	{
		pheno_summary_file <- paste0(QCed_io_name, "_cat_variables_both_sexes_phesant_recodings_remove_sex_specific.", i, "_phenosummary.tsv")

		if(!file.exists(pheno_summary_file))
			next

		pheno_summary <- rbind(pheno_summary, read.table(pheno_summary_file, sep='\t', quote="", comment.char="", header=TRUE, stringsAsFactors=FALSE))
	}
}

pheno_summary_file <- paste0(QCed_io_name, "_cts_both_sexes_phenosummary.tsv")
if(file.exists(pheno_summary_file)) {
	pheno_summary_cts <- read.table(pheno_summary_file, sep='\t', quote="", comment.char="", header=TRUE, stringsAsFactors=FALSE)
	if(!exists("pheno_summary")) {
		pheno_summary <- pheno_summary_cts
	} else{
		pheno_summary <- rbind(pheno_summary, pheno_summary_cts)	
	}
}

# Now, check what's in this file that isn't in the summary files...
colnames(pheno)[which(!(colnames(pheno) %in% rownames(pheno_summary)))]
rownames(pheno_summary)[which(!(rownames(pheno_summary) %in% colnames(pheno)))]

fwrite(pheno_summary, file=paste0(final_output, 'combined_both_sexes_no_sex_specific_summary.tsv'), quote=FALSE, sep='\t', row.names=TRUE)
fwrite(pheno, file=paste0(final_output, 'combined_both_sexes_no_sex_specific.tsv'), quote=FALSE, sep='\t')

rm("pheno")
rm("pheno_summary")

# Males
pheno_file <- paste0(QCed_io_name, "_cat_variables_males.1.tsv")

if(file.exists(pheno_file))
	pheno <- fread(pheno_file, sep='\t', data.table=FALSE, header=TRUE)

if(n_chunks > 1) {
	for(i in 2:n_chunks)
	{
		pheno_file <- paste0(QCed_io_name, "_cat_variables_males.", i, ".tsv")
		
		if(!file.exists(pheno_file))
			next
		
		pheno_tmp <- fread(pheno_file, sep='\t', data.table=FALSE, header=TRUE)
		# check 
		print(all(pheno_tmp[,1] == pheno[,1]))
		pheno <- cbind(pheno, pheno_tmp[,-1, drop=FALSE])
	}
}

# Next the cts raw
if(file.exists(paste0(QCed_io_name, "_cts_raw_males.tsv")))
{
	pheno_tmp <- fread(paste0(QCed_io_name, "_cts_raw_males.tsv"), sep='\t', data.table=FALSE, header=TRUE)

	if(!exists("pheno")) {
		names(pheno_tmp) <- paste0(names(pheno_tmp), '_raw')
		pheno <- pheno_tmp
	} else {
		print(all(order(pheno_tmp[,1]) == order(pheno[,1])))
		names(pheno_tmp) <- paste0(names(pheno_tmp), '_raw')
		pheno <- cbind(pheno, pheno_tmp[,-1, drop=FALSE])
	}
}

# Finally, cts IRNT
if(file.exists(paste0(paste0(QCed_io_name, "_cts_irnt_males.tsv"))))
{
	pheno_tmp <- fread(paste0(QCed_io_name, "_cts_irnt_males.tsv"), sep='\t', data.table=FALSE, header=TRUE)
	
	if(!exists("pheno")) {
		names(pheno_tmp) <- paste0(names(pheno_tmp), '_irnt')
		pheno <- pheno_tmp
	} else {
		print(all(order(pheno_tmp[,1]) == order(pheno[,1])))
		names(pheno_tmp) <- paste0(names(pheno_tmp), '_irnt')
		pheno <- cbind(pheno, pheno_tmp[,-1, drop=FALSE])
	}
}

if(exists("pheno"))
	colnames(pheno)[1] <- 'userId'

# Now, check what's in this file that isn't in the summary files...
pheno_summary_file <- paste0(QCed_io_name, "_cat_variables_males_phesant_recodings_remove_sex_specific.1_phenosummary.tsv")
if(file.exists(pheno_summary_file))
	pheno_summary <- read.table(pheno_summary_file, sep='\t', quote="", comment.char="", header=TRUE, stringsAsFactors=FALSE)

if(n_chunks > 1) {
	for(i in 2:n_chunks)
	{
		pheno_summary_file <- paste0(QCed_io_name, "_cat_variables_males_phesant_recodings_remove_sex_specific.", i, "_phenosummary.tsv")

		if (!file.exists(pheno_summary_file))
			next

		pheno_summary <- rbind(pheno_summary, read.table(pheno_summary_file, sep='\t', quote="", comment.char="", header=TRUE, stringsAsFactors=FALSE))
	}
}

pheno_summary_file <- paste0(QCed_io_name, "_cts_males_phenosummary.tsv")
if(file.exists(pheno_summary_file)) {
	pheno_summary_cts <- read.table(pheno_summary_file, sep='\t', quote="", comment.char="", header=TRUE, stringsAsFactors=FALSE)
	if(!exists("pheno_summary")) {
		pheno_summary <- pheno_summary_cts
	} else{
		pheno_summary <- rbind(pheno_summary, pheno_summary_cts)	
	}
}

colnames(pheno)[which(!(colnames(pheno) %in% rownames(pheno_summary)))]
rownames(pheno_summary)[which(!(rownames(pheno_summary) %in% colnames(pheno)))]

fwrite(pheno_summary, file=paste0(final_output, 'combined_males_summary.tsv'), quote=FALSE, sep='\t', row.names=TRUE)
fwrite(pheno, file=paste0(final_output, 'combined_males.tsv'), quote=FALSE, sep='\t')

rm("pheno")
rm("pheno_summary")

# Females
pheno_file <- paste0(QCed_io_name, "_cat_variables_females.1.tsv")

if(file.exists(pheno_file))
	pheno <- fread(pheno_file, sep='\t', data.table=FALSE, header=TRUE)

if(n_chunks > 1) {
	for(i in 2:n_chunks)
	{
		pheno_file <- paste0(QCed_io_name, "_cat_variables_females.", i, ".tsv")
		
		if(!file.exists(pheno_file))
			next
		
		pheno_tmp <- fread(pheno_file, sep='\t', data.table=FALSE, header=TRUE)
		# check 
		print(all(pheno_tmp[,1] == pheno[,1]))
		pheno <- cbind(pheno, pheno_tmp[,-1, drop=FALSE])
	}
}

# Next the cts raw
if(file.exists(paste0(QCed_io_name, "_cts_raw_females.tsv")))
{
	pheno_tmp <- fread(paste0(QCed_io_name, "_cts_raw_females.tsv"), sep='\t', data.table=FALSE, header=TRUE)

	if(!exists("pheno")) {
		names(pheno_tmp) <- paste0(names(pheno_tmp), '_raw')
		pheno <- pheno_tmp
	} else {
		print(all(order(pheno_tmp[,1]) == order(pheno[,1])))
		names(pheno_tmp) <- paste0(names(pheno_tmp), '_raw')
		pheno <- cbind(pheno, pheno_tmp[,-1, drop=FALSE])
	}
}

# Finally, cts IRNT
if(file.exists(paste0(paste0(QCed_io_name, "_cts_irnt_females.tsv"))))
{
	pheno_tmp <- fread(paste0(QCed_io_name, "_cts_irnt_females.tsv"), sep='\t', data.table=FALSE, header=TRUE)
	
	if(!exists("pheno")) {
		names(pheno_tmp) <- paste0(names(pheno_tmp), '_irnt')
		pheno <- pheno_tmp
	} else {
		print(all(order(pheno_tmp[,1]) == order(pheno[,1])))
		names(pheno_tmp) <- paste0(names(pheno_tmp), '_irnt')
		pheno <- cbind(pheno, pheno_tmp[,-1, drop=FALSE])
	}
}

if(exists("pheno"))
	colnames(pheno)[1] <- 'userId'

# Now, check what's in this file that isn't in the summary files...
pheno_summary_file <- paste0(QCed_io_name, "_cat_variables_females_phesant_recodings_remove_sex_specific.1_phenosummary.tsv")
if(file.exists(pheno_summary_file))
	pheno_summary <- read.table(pheno_summary_file, sep='\t', quote="", comment.char="", header=TRUE, stringsAsFactors=FALSE)

if(n_chunks > 1) {
	for(i in 2:n_chunks)
	{
		pheno_summary_file <- paste0(QCed_io_name, "_cat_variables_females_phesant_recodings_remove_sex_specific.", i, "_phenosummary.tsv")

		if (!file.exists(pheno_summary_file))
			next

		pheno_summary <- rbind(pheno_summary, read.table(pheno_summary_file, sep='\t', quote="", comment.char="", header=TRUE, stringsAsFactors=FALSE))
	}
}

pheno_summary_file <- paste0(QCed_io_name, "_cts_females_phenosummary.tsv")
if(file.exists(pheno_summary_file)) {
	pheno_summary_cts <- read.table(pheno_summary_file, sep='\t', quote="", comment.char="", header=TRUE, stringsAsFactors=FALSE)
	if(!exists("pheno_summary")) {
		pheno_summary <- pheno_summary_cts
	} else{
		pheno_summary <- rbind(pheno_summary, pheno_summary_cts)	
	}
}

colnames(pheno)[which(!(colnames(pheno) %in% rownames(pheno_summary)))]
rownames(pheno_summary)[which(!(rownames(pheno_summary) %in% colnames(pheno)))]

fwrite(pheno_summary, file=paste0(final_output, 'combined_females_summary.tsv'), quote=FALSE, sep='\t', row.names=TRUE)
fwrite(pheno, file=paste0(final_output, 'combined_females.tsv'), quote=FALSE, sep='\t')

rm("pheno")
rm("pheno_summary")

system(paste0("gsutil cp ", final_output, '*combined* gs://phenotype_pharma/PHESANT_output/', n_exomes, "/", date, "/"))
