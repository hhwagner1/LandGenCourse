#### This file contains extra functions needed to 


### AIC weight function

aic_weights <- function(aic_vals) {
	
	min_aic <- which(aic_vals == min(aic_vals))
	
	change_vec <- numeric(length(aic_vals))
	
	for(i in 1:length(aic_vals)) {
		
		change_vec[i] <- exp(-0.5*(aic_vals[i] - aic_vals[min_aic]))
		
		}
	
	fin_out <- change_vec/sum(change_vec)
	
	return(fin_out)
	
	}

### Replication of pop effects for easy adding

pop_rep <- function(pop.eff, n.fam, fam.eff) {
	
	out <- numeric(n.fam)
	
	for(i in 1:nrow(pop.eff)) {
		
		out[grep(pattern = row.names(pop.eff)[i], x = row.names(fam.eff))] <- pop.eff[i,1]
		
		}
	
	return(out)
	
	}
	
### Function to carry out parametric bootstrapping for h2

mod_boot <- function(model, nboot) {
	
	dat <- simulate(model, nboot)
	
	h2_boot <- numeric(nboot)
	
	for(i in 1:ncol(dat)) {
		
		phen_t <- cbind(phen[,c("population", "family", "block")], dat[,i])
		
		out <- lmer(dat[,i] ~ 1 + (1|population/family) + block, data = phen_t, REML = F)
		
		add_var <- 4*VarCorr(out)[[1]][1]
		
		total_wp_var <- VarCorr(out)[[1]][1] + (attr(VarCorr(out),"sc")^2)
		
		h2_boot[i] <- add_var/total_wp_var
		
		}
	
	return(h2_boot)
	
	}

### Function to carry out parametric bootstrapping for qst

mod_boot_qst <- function(model, nboot) {
	
	dat <- simulate(model, nboot)
	
	qst_boot <- numeric(nboot)
	
	for(i in 1:ncol(dat)) {
		
		phen_t <- cbind(phen[,c("population", "family", "block")], dat[,i])
		
		out <- lmer(dat[,i] ~ 1 + (1|population/family) + block, data = phen_t, REML = F)
		
		num_qst <- VarCorr(out)[[2]][1]
		
		dem_qst <- VarCorr(out)[[2]][1] + (8*VarCorr(out)[[1]][1])
		
		qst_boot[i] <- num_qst/dem_qst
		
		}
	
	return(qst_boot)
	
	}

### Function to convert SNP genotypes to FSTAT format. ids should be a vector of id columns (e.g. c(1,2))

hierfstat_convert <- function(snp, ids) {
    
    snp_t <- snp[,-ids]
    
    snp_out <- data.frame(matrix(nrow=nrow(snp_t), ncol = ncol(snp_t)))
    
    for(i in 1:(ncol(snp_t))) {
        
        temp <- strsplit(x = as.character(snp_t[,i]), split="")
        
        temp_table <- table(unlist(temp))
        
        if(length(temp_table) == 1) {snp_out[,i] <- rep("NA", nrow(snp_t))} else {
        
            if(temp_table[1] == temp_table[2]) {min_al <- names(temp_table)[1]} else {min_al <- names(temp_table)[which(temp_table == min(temp_table))]}
        
            if(temp_table[1] == temp_table[2]) {maj_al <- names(temp_table)[2]} else {maj_al <- names(temp_table)[which(temp_table == max(temp_table))]}
        
            temp_dat <- as.character(snp_t[,i])
        
            temp_dat[which(as.character(temp_dat) == paste(min_al,min_al,sep=""))] <- 11
        
            temp_dat[which(as.character(temp_dat) == paste(maj_al,maj_al,sep=""))] <- 22
        
            temp_dat[which(as.character(temp_dat) == paste(min_al,maj_al,sep="") | temp_dat == paste(maj_al,min_al,sep=""))] <- 12
        
            snp_out[,i] <- as.numeric(temp_dat)
        
        }
        
    }

    return(snp_out)
    
    }

### Function to take hierfstat output and get FST per locus. This works only population as a level. No more hierarchy.

fst_persnp <- function(vc, names) {
    
    fst_all <- numeric(nrow(vc))
    
    names(fst_all) <- names
    
    for(i in 1:nrow(vc)) {
        
        fst_all[i] <- vc[i,1]/sum(vc[i,])
        
        }
    
    return(fst_all)
    
    }

### Function to get expected heterozygosity

het_snp <- function(snp, finite.cor, names) {
    
    if(finite.cor == "TRUE") {
    
        het_out <- numeric(ncol(snp))
        
        names(het_out) <- names
        
        for(i in 1:ncol(snp)) {
            
            homo11 <- length(which(snp[,i] == 11))
            
            het12 <- length(which(snp[,i] == 12))
            
            homo22 <- length(which(snp[,i] == 22))
            
            alf1 <- ((2*homo11) + het12)/(2*(homo11 + het12 + homo22))
            
            alf2 <- 1 - alf1
            
            tot <- (2*(homo11 + het12 + homo22))
            
            het_out[i] <- 2*alf1*alf2*(tot/(tot-1))
            
            }
    
        return(het_out)
    
        } else {
        
        het_out <- numeric(ncol(snp))
        
        names(het_out) <- names
        
        for(i in 1:ncol(snp)) {
            
            homo11 <- length(which(snp[,i] == 11))
            
            het12 <- length(which(snp[,i] == 12))
            
            homo22 <- length(which(snp[,i] == 22))
            
            alf1 <- ((2*homo11) + het12)/(2*(homo11 + het12 + homo22))
            
            alf2 <- 1 - alf1
            
            tot <- (2*(homo11 + het12 + homo22))
            
            het_out[i] <- 2*alf1*alf2
        
            }
    
        return(het_out)
    
        }
        
    }

### Function to reformat SNP data to geno format for LEA

geno_reformat <- function(snp) {
    
    reformat <- matrix(nrow = ncol(snp), ncol = nrow(snp))
    
    for(i in 1:ncol(snp)) {
        
        reformat[i,which(snp[,i] == 11)] <- 2
        
        reformat[i,which(snp[,i] == 12)] <- 1
        
        reformat[i,which(snp[,i] == 22)] <- 0
        
        reformat[i,which(is.na(snp[,i]) == "TRUE")] <- 9
        
    }
    
    return(reformat)
    
}
    




































