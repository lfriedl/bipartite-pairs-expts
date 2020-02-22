
# This file: remove calls to model2 and model4, and clean up. 
# (analytics2.R preserves all that other code, which works. I just want it out of the way.)
# -Also remove "onlyDups" code, and "given that the item is non-zero" code. Out of scope for now.
# -Some functions still have multiple forms, for semi-good reasons: 
#  -either a perCmpt version and one for a whole pi_vector (which may simply sum the 
#  perCmpt calls), 
#  -or a pi_vector version and a const_pi version (which should be more efficient because 
#  it multiplies things by n instead of making n calls).

# -Have changed all examples of "pi" to "p_i", so the constant doesn't get redefined.
# -Have changed all log() calls to log2(). Shouldn't affect LRs or AUCs, but would affect 
# expected values and variances.

# probs for a single component under a given model
# Independent Bernoulli / negs model
p11_neg = function(p_i) {
	return(p_i*p_i)
}

p10_neg = function(p_i) {
	return(2 * p_i*(1-p_i))
}

p00_neg = function(p_i) {
	return((1-p_i)**2)
}

p11_pos5 = function(p_i, t) {
	return(p_i * (t + (1-t) * p_i))
}

p10_pos5 = function(p_i, t) {
	return(2 * p_i * (1 - p_i) * (1 - t))
}

p00_pos5 = function(p_i, t) {
	return((1 - p_i) * (1 - p_i + p_i * t))
}

# LR's for a single component
LR11_pos5 = function(p_i, t) {
	return(p11_pos5(p_i, t) / p11_neg(p_i))
}
LR10_pos5 = function(p_i, t) {
	return(p10_pos5(p_i, t) / p10_neg(p_i))
}
LR00_pos5 = function(p_i, t) {
	return(p00_pos5(p_i, t) / p00_neg(p_i))
}

# Do estimate symbolically, assuming distributions are normals
AUC_template_theor = function(posE, posVar, negE, negVar) {
	return(pnorm((posE - negE) / sqrt(posVar + negVar)) )
}

# Do estimate through simulation of normals
AUC_template_sim = function(posE, posVar, negE, negVar, nSamples = 1000) {
	library(pROC)   # caution: has namespace conflicts with stats package and my evals package

	return(auc(roc(controls=rnorm(nSamples, mean=negE, sd=sqrt(negVar)),
			cases = rnorm(nSamples, mean=posE, sd=sqrt(negVar)))))
}

# --- Model 5 scoring ---

# ELLRs, VarLLRs per component
ELLRPos_pos5_perCmpt = function(p_i, t) {
	p11 = (p11_pos5(p_i, t)) * log2(LR11_pos5(p_i, t))
	p10 = (p10_pos5(p_i, t)) * log2(LR10_pos5(p_i, t))
	p00 = (p00_pos5(p_i, t)) * log2(LR00_pos5(p_i, t))
	return(p11 + p10 + p00)
}

ELLRNeg_pos5_perCmpt = function(p_i, t) {
	p11 = (p11_neg(p_i)) * log2(LR11_pos5(p_i, t))
	p10 = (p10_neg(p_i)) * log2(LR10_pos5(p_i, t))
	p00 = (p00_neg(p_i)) * log2(LR00_pos5(p_i, t))
	return(p11 + p10 + p00)
}

VarLLRPos_pos5_perCmpt = function(p_i, t) {
	ex_per_cmpt = ELLRPos_pos5_perCmpt(p_i, t)
	p11 = p11_pos5(p_i, t) * (log2(LR11_pos5(p_i, t)) - ex_per_cmpt)**2
	p10 = p10_pos5(p_i, t) * (log2(LR10_pos5(p_i, t)) - ex_per_cmpt)**2
	p00 = p00_pos5(p_i, t) * (log2(LR00_pos5(p_i, t)) - ex_per_cmpt)**2
	return(p11 + p10 + p00)
}

VarLLRNeg_pos5_perCmpt = function(p_i, t) {
	ex_per_cmpt = ELLRNeg_pos5_perCmpt(p_i, t)
	p11 = p11_neg(p_i) * (log2(LR11_pos5(p_i, t)) - ex_per_cmpt)**2
	p10 = p10_neg(p_i) * (log2(LR10_pos5(p_i, t)) - ex_per_cmpt)**2
	p00 = p00_neg(p_i) * (log2(LR00_pos5(p_i, t)) - ex_per_cmpt)**2
	return(p11 + p10 + p00)
}

KLdiv_pos5_perCmpt = function(p_i, t) {
	return(ELLRPos_pos5_perCmpt(p_i, t) - ELLRNeg_pos5_perCmpt(p_i, t))
}
KLdiv_pos5 = function(pi_vector, t) {
	return(sum(mapply(KLdiv_pos5_perCmpt, pi_vector, t)))
}

AUC_pos5 = function(pi_vector, trueT, tHat=NULL) {
    if (is.null(tHat)) {
        tHat = trueT
    }

	posE = sum(mapply(ELLRPos_pos5_perCmpt_mismatch, pi_vector, trueT, tHat))
	posVar = sum(mapply(VarLLRPos_pos5_perCmpt_mismatch, pi_vector, trueT, tHat))
	negE = sum(mapply(ELLRNeg_pos5_perCmpt, pi_vector, tHat))
	negVar = sum(mapply(VarLLRNeg_pos5_perCmpt, pi_vector, tHat))

	return(AUC_template_theor(posE, posVar, negE, negVar))
}
AUC_pos5_constpi = function(p_i, n, trueT, tHat=NULL) {
    if (is.null(tHat)) {
        tHat = trueT
    }
    posE = n * ELLRPos_pos5_perCmpt_mismatch(p_i, trueT, tHat)
    posVar = n * VarLLRPos_pos5_perCmpt_mismatch(p_i, trueT, tHat)
    negE = n * ELLRNeg_pos5_perCmpt(p_i, tHat)
    negVar = n * VarLLRNeg_pos5_perCmpt(p_i, tHat)

    return(AUC_template_theor(posE, posVar, negE, negVar))
}


# tHat mismatches: generalization of earlier functions 
ELLRPos_pos5_perCmpt_mismatch = function(p_i, trueT, tHat) {
    p11 = (p11_pos5(p_i, trueT)) * log2(LR11_pos5(p_i, tHat))
    p10 = (p10_pos5(p_i, trueT)) * log2(LR10_pos5(p_i, tHat))
    if (trueT == 1) { 
        p10 = 0 
    }
    p00 = (p00_pos5(p_i, trueT)) * log2(LR00_pos5(p_i, tHat))
    return(p11 + p10 + p00)
}

VarLLRPos_pos5_perCmpt_mismatch = function(p_i, trueT, tHat) {
    ex_per_cmpt = ELLRPos_pos5_perCmpt_mismatch(p_i, trueT, tHat)
    p11 = p11_pos5(p_i, trueT) * (log2(LR11_pos5(p_i, tHat)) - ex_per_cmpt)**2
    p10 = p10_pos5(p_i, trueT) * (log2(LR10_pos5(p_i, tHat)) - ex_per_cmpt)**2
    if (trueT == 1) { 
        p10 = 0 
    }
    p00 = p00_pos5(p_i, trueT) * (log2(LR00_pos5(p_i, tHat)) - ex_per_cmpt)**2
    return(p11 + p10 + p00)
}
KLdiv_pos5_perCmpt_mismatch = function(p_i, trueT, tHat) {
    # note: neg distribution doesn't care what true t is
    return(ELLRPos_pos5_perCmpt_mismatch(p_i, trueT, tHat) - ELLRNeg_pos5_perCmpt(p_i, tHat))
}
KLdiv_pos5_mismatch = function(pi_vector, trueT, tHat) {
    return(sum(mapply(KLdiv_pos5_perCmpt_mismatch, pi_vector, trueT, tHat)))
}


# -- Pearson weighted score, under model5 positives --
pearWeightScore11 = function(p_i) {
    return((1-p_i)/p_i)
}
pearWeightScore10 = function() {
    return(-1)
}
pearWeightScore00 = function(p_i) {
    return(p_i/(1-p_i))
}

# should/does always return 0 (+/- floating point precision errors)
EPearWeightNeg_perCmpt = function(p_i) {
    return(p11_neg(p_i) * pearWeightScore11(p_i) + p10_neg(p_i) * pearWeightScore10() + p00_neg(p_i) * pearWeightScore00(p_i))
}
# should/does always return 1
VarPearWeightNeg_perCmpt = function(p_i) {
    ex_per_cmpt = EPearWeightNeg_perCmpt(p_i)
    var11 = p11_neg(p_i) * (pearWeightScore11(p_i) - ex_per_cmpt)**2
    var10 = p10_neg(p_i) * (pearWeightScore10() - ex_per_cmpt)**2
    var00 = p00_neg(p_i) * (pearWeightScore00(p_i) - ex_per_cmpt)**2
    return(var11 + var10 + var00)
}
# should/does always return t
EPearWeightPos_perCmpt = function(p_i, t) {
    return(p11_pos5(p_i, t) * pearWeightScore11(p_i) + p10_pos5(p_i, t) * pearWeightScore10() + p00_pos5(p_i, t) * pearWeightScore00(p_i))
}
# should/does always return a complicated function: 1 - t - t**2 + t((1-p)**3 + p**3)/(p*(1-p))
VarPearWeightPos_perCmpt = function(p_i, t) {
    ex_per_cmpt = EPearWeightPos_perCmpt(p_i, t)
    var11 = p11_pos5(p_i, t) * (pearWeightScore11(p_i) - ex_per_cmpt)**2
    var10 = p10_pos5(p_i, t) * (pearWeightScore10() - ex_per_cmpt)**2
    var00 = p00_pos5(p_i, t) * (pearWeightScore00(p_i) - ex_per_cmpt)**2
    return(var11 + var10 + var00)
}
AUCPearWeight_pos5_constpi = function(p_i, trueT, n) {
    posE = n * EPearWeightPos_perCmpt(p_i, trueT)
    posVar = n * VarPearWeightPos_perCmpt(p_i, trueT)
    negE = n * EPearWeightNeg_perCmpt(p_i)
    negVar = n * VarPearWeightNeg_perCmpt(p_i)
    return(AUC_template_theor(posE, posVar, negE, negVar))
}    
AUCPearWeight_pos5 = function(pi_vector, trueT) {
    posE = sum(mapply(EPearWeightPos_perCmpt, pi_vector, trueT))
    posVar = sum(mapply(VarPearWeightPos_perCmpt, pi_vector, trueT))
    negE = sum(mapply(EPearWeightNeg_perCmpt, pi_vector))
    negVar = sum(mapply(VarPearWeightNeg_perCmpt, pi_vector))
    return(AUC_template_theor(posE, posVar, negE, negVar))
}

# --- m11 model ---
LR11_m11 = function(p_i) {
	return(1/p_i)
}
# LR10_m11 == LR00_m11 = 1

ELLRNeg_m11_perCmpt = function(p_i) {
	return(p11_neg(p_i) * log2(LR11_m11(p_i)))
	# since log2(LR10_m11) == log2(LR00_m11) == 0
}
VarLLRNeg_m11_perCmpt = function(p_i) {
	ex_per_cmpt = ELLRNeg_m11_perCmpt(p_i)
	p11 = p11_neg(p_i) * (log2(LR11_m11(p_i)) - ex_per_cmpt)**2
	p10 = p10_neg(p_i) * (0 - ex_per_cmpt)**2
	p00 = p00_neg(p_i) * (0 - ex_per_cmpt)**2
	return(p11 + p10 + p00)
}


ELLRPos5_m11_perCmpt = function(p_i, t) {
	return(p11_pos5(p_i, t) * log2(LR11_m11(p_i)))
}
VarLLRPos5_m11_perCmpt = function(p_i, t) {
	ex_per_cmpt = ELLRPos5_m11_perCmpt(p_i, t)
	p11 = (p11_pos5(p_i, t)) * (log2(LR11_m11(p_i)) - ex_per_cmpt)**2
	p10 = (p10_pos5(p_i, t)) * (0 - ex_per_cmpt)**2
	p00 = (p00_pos5(p_i, t)) * (0 - ex_per_cmpt)**2
	return(p11 + p10 + p00)
}
AUC_pos5_m11 = function(pi_vector, trueT) {
	posE = sum(mapply(ELLRPos5_m11_perCmpt, pi_vector, trueT))
	posVar = sum(mapply(VarLLRPos5_m11_perCmpt, pi_vector, trueT))
	negE = sum(mapply(ELLRNeg_m11_perCmpt, pi_vector))
	negVar = sum(mapply(VarLLRNeg_m11_perCmpt, pi_vector))
	return(AUC_template_theor(posE, posVar, negE, negVar))
}
AUC_pos5_m11_constpi = function(p_i, n, trueT) {
    posE = n * ELLRPos5_m11_perCmpt(p_i, trueT)
    posVar = n * VarLLRPos5_m11_perCmpt(p_i, trueT)
    negE = n * ELLRNeg_m11_perCmpt(p_i) 
    negVar = n * VarLLRNeg_m11_perCmpt(p_i) 
    
    return(AUC_template_theor(posE, posVar, negE, negVar))
}


KLdivPos5_m11_perCmpt = function(p_i, t) {
	return(ELLRPos5_m11_perCmpt(p_i, t) - ELLRNeg_m11_perCmpt(p_i))
}
KLdivPos5_m11 = function(pi_vector, t) {
	return(sum(mapply(KLdivPos5_m11_perCmpt, pi_vector, t)))
}
	
# --- m1100 model ---
LR11_m1100 = function(p_i) {
	return(1/p_i)
}
LR00_m1100 = function(p_i) {
	return(1/(1-p_i))
}
# LR10_m1100 = 1

ELLRNeg_m1100_perCmpt = function(p_i) {
	p11 = p11_neg(p_i) * log2(LR11_m1100(p_i))
	p00 = p00_neg(p_i) * log2(LR00_m1100(p_i))
	return(p11 + p00)
	# since log2(LR10_m11) == 0
}
VarLLRNeg_m1100_perCmpt = function(p_i) {
	ex_per_cmpt = ELLRNeg_m1100_perCmpt(p_i)
	p11 = p11_neg(p_i) * (log2(LR11_m1100(p_i)) - ex_per_cmpt)**2
	p10 = p10_neg(p_i) * (0 - ex_per_cmpt)**2
	p00 = p00_neg(p_i) * (log2(LR00_m1100(p_i)) - ex_per_cmpt)**2
	return(p11 + p10 + p00)
}


ELLRPos5_m1100_perCmpt = function(p_i, t) {
	p11 = p11_pos5(p_i, t) * log2(LR11_m1100(p_i))
	p00 = p00_pos5(p_i, t) * log2(LR00_m1100(p_i))
	return(p11 + p00)
}
VarLLRPos5_m1100_perCmpt = function(p_i, t) {
	ex_per_cmpt = ELLRPos5_m1100_perCmpt(p_i, t)
	p11 = (p11_pos5(p_i, t)) * (log2(LR11_m1100(p_i)) - ex_per_cmpt)**2
	p10 = (p10_pos5(p_i, t)) * (0 - ex_per_cmpt)**2
	p00 = (p00_pos5(p_i, t)) * (log2(LR00_m1100(p_i)) - ex_per_cmpt)**2
	return(p11 + p10 + p00)
}
AUC_pos5_m1100 = function(pi_vector, trueT) {
	posE = sum(mapply(ELLRPos5_m1100_perCmpt, pi_vector, trueT))
	posVar = sum(mapply(VarLLRPos5_m1100_perCmpt, pi_vector, trueT))
	negE = sum(mapply(ELLRNeg_m1100_perCmpt, pi_vector))
	negVar = sum(mapply(VarLLRNeg_m1100_perCmpt, pi_vector))
	return(AUC_template_theor(posE, posVar, negE, negVar))
}
AUC_pos5_m1100_constpi = function(p_i, n, trueT) {
    posE = n * ELLRPos5_m1100_perCmpt(p_i, trueT)
    posVar = n * VarLLRPos5_m1100_perCmpt(p_i, trueT)
    negE = n * ELLRNeg_m1100_perCmpt(p_i)
    negVar = n * VarLLRNeg_m1100_perCmpt(p_i)
    return(AUC_template_theor(posE, posVar, negE, negVar))
}


KLdivPos5_m1100_perCmpt = function(p_i, t) {
	return(ELLRPos5_m1100_perCmpt(p_i, t) - ELLRNeg_m1100_perCmpt(p_i))
}
KLdivPos5_m1100 = function(pi_vector, t) {
	return(sum(mapply(KLdivPos5_m1100_perCmpt, pi_vector, t)))
}

# plain old m (# 1/1s)
EPos5_m_perCmpt = function(p_i, t) {
    return(p11_pos5(p_i, t) * 1)
}

ENeg_m_perCmpt = function(p_i, t) {
    return(p11_neg(p_i) * 1)
}

VarPos5_m_perCmpt = function(p_i, t) {
    ex_per_cmpt = EPos5_m_perCmpt(p_i, t)
    p11 = p11_pos5(p_i, t) * (1 - ex_per_cmpt)**2
    p10 = p10_pos5(p_i, t) * (0 - ex_per_cmpt)**2
    p00 = p00_pos5(p_i, t) * (0 - ex_per_cmpt)**2
    return(p11 + p10 + p00)
}

VarNeg_m_perCmpt = function(p_i) {
    ex_per_cmpt = ENeg_m_perCmpt(p_i)
    p11 = p11_neg(p_i) * (1 - ex_per_cmpt)**2
    p10 = p10_neg(p_i) * (0 - ex_per_cmpt)**2
    p00 = p00_neg(p_i) * (0 - ex_per_cmpt)**2
    return(p11 + p10 + p00)
}
AUC_pos5_m_constpi = function(p_i, n, trueT) {
    posE = n * EPos5_m_perCmpt(p_i, trueT)
    posVar = n * VarPos5_m_perCmpt(p_i, trueT)
    negE = n * ENeg_m_perCmpt(p_i)
    negVar = n * VarNeg_m_perCmpt(p_i)
    return(AUC_template_theor(posE, posVar, negE, negVar))
}

# plain old d (# 1/1s)
EPos5_d_perCmpt = function(p_i, t) {
    return(p10_pos5(p_i, t) * -1)
}

ENeg_d_perCmpt = function(p_i) {
    return(p10_neg(p_i) * -1)
}

VarPos5_d_perCmpt = function(p_i, t) {
    ex_per_cmpt = EPos5_d_perCmpt(p_i, t)
    p11 = p11_pos5(p_i, t) * (0 - ex_per_cmpt)**2
    p10 = p10_pos5(p_i, t) * (-1 - ex_per_cmpt)**2
    p00 = p00_pos5(p_i, t) * (0 - ex_per_cmpt)**2
    return(p11 + p10 + p00)
}

VarNeg_d_perCmpt = function(p_i) {
    ex_per_cmpt = ENeg_d_perCmpt(p_i)
    p11 = p11_neg(p_i) * (0 - ex_per_cmpt)**2
    p10 = p10_neg(p_i) * (-1 - ex_per_cmpt)**2
    p00 = p00_neg(p_i) * (0 - ex_per_cmpt)**2
    return(p11 + p10 + p00)
}

AUC_pos5_d_constpi = function(p_i, n, trueT) {
    posE = n * EPos5_d_perCmpt(p_i, trueT)
    posVar = n * VarPos5_d_perCmpt(p_i, trueT)
    negE = n * ENeg_d_perCmpt(p_i)
    negVar = n * VarNeg_d_perCmpt(p_i)
    return(AUC_template_theor(posE, posVar, negE, negVar))
}

# Adamic/Adar
adamicScore11 = function(p_i, m) {
    return(1/log2(m * p_i))
}

EPos5_adamic_perCmpt = function(p_i, m, t) {
    return(p11_pos5(p_i, t) * adamicScore11(p_i, m))
}

ENeg_adamic_perCmpt = function(p_i, m) {
    return(p11_neg(p_i) * adamicScore11(p_i, m))
}
VarPos5_adamic_perCmpt = function(p_i, m, t) {
    ex_per_cmpt = EPos5_adamic_perCmpt(p_i, m, t)
    p11 = p11_pos5(p_i, t) * (adamicScore11(p_i, m) - ex_per_cmpt)**2
    p10 = p10_pos5(p_i, t) * (0 - ex_per_cmpt)**2
    p00 = p00_pos5(p_i, t) * (0 - ex_per_cmpt)**2
    return(p11 + p10 + p00)
}

VarNeg_adamic_perCmpt = function(p_i, m) {
    ex_per_cmpt = ENeg_adamic_perCmpt(p_i, m)
    p11 = p11_neg(p_i) * (adamicScore11(p_i, m) - ex_per_cmpt)**2
    p10 = p10_neg(p_i) * (0 - ex_per_cmpt)**2
    p00 = p00_neg(p_i) * (0 - ex_per_cmpt)**2
    return(p11 + p10 + p00)
}
AUC_pos5_adamic_constpi = function(p_i, n, m, trueT) {
    posE = n * EPos5_adamic_perCmpt(p_i, m, trueT)
    posVar = n * VarPos5_adamic_perCmpt(p_i, m, trueT)
    negE = n * ENeg_adamic_perCmpt(p_i, m)
    negVar = n * VarNeg_adamic_perCmpt(p_i, m)
    return(AUC_template_theor(posE, posVar, negE, negVar))
}

entropy_phi_constpi = function(p_i, n) {
	p1 = p_i * log2(p_i)
	p0 = (1 - p_i) * log2(1 - p_i)
	return(-1 * n * (p1 + p0))
}

entropy_phi = function(pi_vector) {
	return(sum(mapply(entropy_phi_constpi, pi_vector, n=1)))
}
collision_entropy_perCmpt = function(p_i) {
    return(-1 * log2(p_i**2 + (1 - p_i)**2))
}
collision_entropy = function(pi_vector) {
	return(sum(mapply(collision_entropy_perCmpt, pi_vector)))
}
    
convertTToModel5 = function(tForModel2_4, pi_vector) {
    tForModel5 = (sum(pi_vector) * tForModel2_4 - sum(pi_vector**2)) / (sum(pi_vector) - sum(pi_vector**2))
    return(tForModel5)
}

convertTToModel5ConstPi = function(tForModel2_4, p_i) {
    tForModel5 = (p_i * tForModel2_4 - (p_i**2)) / (p_i - (p_i**2))
    return(tForModel5)
}

convertTFromModel5 = function(tForModel5, pi_vector) {
    tForModel2_4 = (sum(pi_vector) * tForModel5 + sum(pi_vector**2) * (1 - tForModel5)) / sum(pi_vector)
    return(tForModel2_4)
}

convertTFromModel5ConstPi = function(tForModel5, p_i) {
    tForModel2_4 = (p_i * tForModel5 + (p_i**2) * (1 - tForModel5)) / p_i
    return(tForModel2_4)
}

# lacking a better word for it. Hopefully, matches KLdiv diff when t = 1.
entropyish_phisq = function(p_i, n) {
	return(-1 * n * (p_i - p_i**2) * log2(p_i - p_i**2))
}

# --- IDF model, given constant p_i (so IDF weights cancel out) == cosine distance ---
cosineScorePair = function(a_11, b_10, c_01) {
	return (a_11 / sqrt((a_11 + b_10) * (a_11 + c_01)))
}

# Try it defining cosine(*, 0) = 0
# only accepts d_00 for historical/compatibility purposes
cosineScore2 = function(p, a_11, b_10, c_01, d_00) {
    if (a_11 + b_10 == 0 || a_11 + c_01 == 0) {
        return(0)
    }
    return(cosineScorePair(a_11, b_10, c_01))
}

# note atypical definitions: jaccard(0, *) = 0
jaccardScorePair = function(a_11, b_10, c_01, d_00) {
    if (a_11 + b_10 == 0 || a_11 + c_01 == 0) {
        return(0)
    }
	return (a_11 / (a_11 + b_10 + c_01))
}

# note atypical definitions: pearson(0, *) = 0
pearsonCorrPair = function(a_11, b_10, c_01, d_00) {
    # for binary variables, pearson is same as phi coefficient (see wikipedia)
    mar1_ = a_11 + b_10
    mar0_ = c_01 + d_00
    mar_1 = a_11 + c_01
    mar_0 = b_10 + d_00
    if (mar1_ == 0 || mar0_ == 0 || mar_1 == 0 || mar_0 == 0) {
        return(0)
    }
    return(((a_11 * d_00) - (b_10 * c_01)) / sqrt(mar1_ * mar0_ * mar_1 * mar_0))
}

probEitherItemAllZeroesGivenNeg = function(p_i, n) {
	totProb0s = 0

	for (a in 0:n) {
		for (b in 0:(n-a)) {
			for (c in 0:(n-a-b)) {

				# We can't have a + b = 0 or a + c = 0. I think those elements were (at some point?) removed from the data set in practice.
				# Possibly, every P(full config) should have been normalized by 1 - P(all vector entries are zero), in all the computations.
				if (a + b == 0 || a + c == 0) {
					numWaysThisCombo = choose(n, a+b+c) * choose(a+b+c, a) * choose(b+c, b)
					pComboForNegs = p11_neg(p_i)**a * (p10_neg(p_i)/2)**(b+c) * p00_neg(p_i)**(n - a - b - c)
					
					totProb0s = totProb0s + (pComboForNegs * numWaysThisCombo)
				}
			}
		}
	}
	return(totProb0s)
}

# distribution of negative scores doesn't depend on positive model
ENeg_cosineScore = function(p_i, n, useScore2 = F, dontNormalize=F) {

    if (dontNormalize) {
        totProb0s = 0
    } else {
    	# first, get the total P(a+b == 0 || a+c == 0). We will have to normalize by this, since we're excluding 
    	# this event from the total (it would give a NaN cosine score).
    	totProb0s = probEitherItemAllZeroesGivenNeg(p_i, n)
    }

	# sum over combos a, b, c: (num ways to get a combo) * (probability of each such config under neg model) * (score assigned to combo)
	totalEVal = 0
	debugging = F
	if (debugging) {
		diagNumWays = c()
		diagProb = c()
		diagScore = c()
		diagA = c()
		diagB = c()
		diagC = c()
	}

	for (a in 0:n) {
		for (b in 0:(n-a)) {
			for (c in 0:(n-a-b)) {

				# We can't have a + b = 0 or a + c = 0. I think those elements were (at some point?) removed from the data set in practice.
				# Every P(full config of pair) should have been normalized by 1 - P(all vector entries are zero in one of the items), in all the computations.
				if ((a + b == 0 || a + c == 0) && !dontNormalize) {
					next
				}

				# this will never scale. Use wise helper functions.
				#numWaysThisCombo = factorial(n) / (factorial(a) * factorial(b) * factorial(c) * factorial(n - a - b- c))

				# I worked out the formula, and it also makes sense intuitively
				numWaysThisCombo = choose(n, a+b+c) * choose(a+b+c, a) * choose(b+c, b)

				# note: we have to divide p10_neg by 2, because it evaluates to 2 * p_i * (1 - p_i), appropriate only when we don't 
				# distinguish events b and c
				pComboForNegs = p11_neg(p_i)**a * (p10_neg(p_i)/2)**(b+c) * p00_neg(p_i)**(n - a - b - c) / (1 - totProb0s)

				if (useScore2) {
					score = cosineScore2(a_11 = a, b_10 = b, c_01 = c, d_00 = (n - a - b - c))
				} else {
					score = cosineScorePair(a_11 = a, b_10 = b, c_01 = c)
				}

				newAmount = numWaysThisCombo * pComboForNegs * score
				totalEVal = totalEVal + newAmount
				if (debugging) {
					diagNumWays= c(diagNumWays, numWaysThisCombo)
					diagProb = c(diagProb, pComboForNegs)
					diagScore = c(diagScore, score)
					diagA = c(diagA, a)
					diagB = c(diagB, b)
					diagC = c(diagC, c)
				}
			}
		}
	}
	return(totalEVal)
}

# only difference between this and ENeg_cosineScore: the score here = (old score - ENeg_cosineScore)**2
VarNeg_cosineScore = function(p_i, n, useScore2 = F, dontNormalize=F) {
	#expVal = ENeg_cosineScore(p_i, n, useScore2)
	expVal = ENeg_cosineScore(p_i, n, useScore2, dontNormalize=dontNormalize)  # doesn't the dontNormalize flag need to be passed along??

	if (dontNormalize) {
	    totProb0s = 0
	} else {
    	# first, get the total P(a+b == 0 || a+c == 0). We will have to normalize by this, since we're excluding 
    	# this event from the total (it would give a NaN cosine score).
    	totProb0s = probEitherItemAllZeroesGivenNeg(p_i, n)
	}

	# sum over combos a, b, c: (num ways to get a combo) * (probability of each such config under neg model) * (score assigned to combo)
	totalEVal = 0
	debugging = F

	for (a in 0:n) {
		for (b in 0:(n-a)) {
			for (c in 0:(n-a-b)) {

				# We can't have a + b = 0 or a + c = 0. I think those elements were (at some point?) removed from the data set in practice.
				# Every P(full config of pair) should have been normalized by 1 - P(all vector entries are zero in one of the items), in all the computations.
			    if ((a + b == 0 || a + c == 0) && !dontNormalize) {
                    next
				}

				# this will never scale. Use wise helper functions.
				#numWaysThisCombo = factorial(n) / (factorial(a) * factorial(b) * factorial(c) * factorial(n - a - b- c))

				# I worked out the formula, and it also makes sense intuitively
				numWaysThisCombo = choose(n, a+b+c) * choose(a+b+c, a) * choose(b+c, b)

				# note: we have to divide p10_neg by 2, because it evaluates to 2 * p_i * (1 - p_i), appropriate only when we don't 
				# distinguish events b and c
				pComboForNegs = p11_neg(p_i)**a * (p10_neg(p_i)/2)**(b+c) * p00_neg(p_i)**(n - a - b - c) / (1 - totProb0s)

				if (useScore2) {
					score = (cosineScore2(a_11 = a, b_10 = b, c_01 = c, d_00 = (n - a - b - c)) - expVal)**2
				} else {
					score = (cosineScorePair(a_11 = a, b_10 = b, c_01 = c) - expVal)**2
				}

				newAmount = numWaysThisCombo * pComboForNegs * score
				totalEVal = totalEVal + newAmount
			}
		}
	}
	return(totalEVal)
}

probEitherItemAllZeroesGivenPos5 = function(p_i, n, t) {
	totProb0s = 0

	for (a in 0:n) {
		for (b in 0:(n-a)) {
			for (c in 0:(n-a-b)) {

				# We can't have a + b = 0 or a + c = 0. I think those elements were (at some point?) removed from the data set in practice.
				# Possibly, every P(full config) should have been normalized by 1 - P(all vector entries are zero), in all the computations.
			    if (a + b == 0 || a + c == 0) {
					numWaysThisCombo = choose(n, a+b+c) * choose(a+b+c, a) * choose(b+c, b)
					pComboForPos = p11_pos5(p_i, t)**a * (p10_pos5(p_i, t)/2)**(b+c) * p00_pos5(p_i, t)**(n - a - b - c)
					
					totProb0s = totProb0s + (pComboForPos * numWaysThisCombo)
				}
			}
		}
	}
	return(totProb0s)
}

# For comments, see ENeg_cosineScore, which is identical except for the probability calc
EPos5_cosineScore = function(p_i, n, t, useScore2 = F, dontNormalize=F) {
	debugging = F

	if (dontNormalize) {
	    totProb0s = 0
	} else {
    	totProb0s = probEitherItemAllZeroesGivenPos5(p_i, n, t)
	}

	# sum over combos a, b, c: (num ways to get a combo) * (probability of each such config under neg model) * (score assigned to combo)
	totalEVal = 0
	if (debugging) {
		diagNumWays = c()
		diagProb = c()
		diagScore = c()
		diagA = c()
		diagB = c()
		diagC = c()
	}

	for (a in 0:n) {
		for (b in 0:(n-a)) {
			for (c in 0:(n-a-b)) {

			    if ((a + b == 0 || a + c == 0) && !dontNormalize) {
					next
				}

				numWaysThisCombo = choose(n, a+b+c) * choose(a+b+c, a) * choose(b+c, b)
				pComboForPos = p11_pos5(p_i, t)**a * (p10_pos5(p_i, t)/2)**(b+c) * p00_pos5(p_i, t)**(n - a - b - c) / (1 - totProb0s)
				if (useScore2) {
					score = cosineScore2(a_11 = a, b_10 = b, c_01 = c, d_00 = (n - a - b - c))
				} else {
					score = cosineScorePair(a_11 = a, b_10 = b, c_01 = c)
				}
				newAmount = numWaysThisCombo * pComboForPos * score
				totalEVal = totalEVal + newAmount

				if (debugging) {
					diagNumWays= c(diagNumWays, numWaysThisCombo)
					diagProb = c(diagProb, pComboForPos)
					diagScore = c(diagScore, score)
					diagA = c(diagA, a)
					diagB = c(diagB, b)
					diagC = c(diagC, c)
				}
			}
		}
	}
	return(totalEVal)
}

VarPos5_cosineScore = function(p_i, n, t, useScore2 = F, dontNormalize=F) {
	#expVal = EPos5_cosineScore(p_i, n, t, useScore2)
	expVal = EPos5_cosineScore(p_i, n, t, useScore2, dontNormalize=dontNormalize)  # doesn't the dontNormalize flag need to be passed along??

	if (dontNormalize) {
	    totProb0s = 0
	} else {
    	totProb0s = probEitherItemAllZeroesGivenPos5(p_i, n, t)
	}
	totalEVal = 0

	for (a in 0:n) {
		for (b in 0:(n-a)) {
			for (c in 0:(n-a-b)) {

			    if ((a + b == 0 || a + c == 0) && !dontNormalize) {
					next
				}

				numWaysThisCombo = choose(n, a+b+c) * choose(a+b+c, a) * choose(b+c, b)
				pComboForPos = p11_pos5(p_i, t)**a * (p10_pos5(p_i, t)/2)**(b+c) * p00_pos5(p_i, t)**(n - a - b - c) / (1 - totProb0s)
				if (useScore2) {
					score = (cosineScore2(a_11 = a, b_10 = b, c_01 = c, d_00 = (n - a - b - c)) - expVal)**2
				} else {
					score = (cosineScorePair(a_11 = a, b_10 = b, c_01 = c) - expVal)**2
				}
				newAmount = numWaysThisCombo * pComboForPos * score
				totalEVal = totalEVal + newAmount

			}
		}
	}
	return(totalEVal)
}

AUC_pos5_cosine_constpi = function(p_i, n, trueT, useCos0=F) {
    posE = EPos5_cosineScore(p_i, n, trueT, useScore2 = useCos0, dontNormalize=useCos0)
    posVar = VarPos5_cosineScore(p_i, n, trueT, useScore2 = useCos0, dontNormalize=useCos0)
    negE = ENeg_cosineScore(p_i, n, useScore2 = useCos0, dontNormalize=useCos0)
    negVar = VarNeg_cosineScore(p_i, n, useScore2 = useCos0, dontNormalize=useCos0)
    return(AUC_template_theor(posE, posVar, negE, negVar))
}

# For a first pass, I won't implement the "normalizing" bit, but will assume functions 
# are defined for all vectors.
AUC_pos5_combinatorialScoreFn = function(p_i, n, trueT, scoreFnName = "cosineScore2") {
    # R is slow at loops, so this function will only go through all combos once.
    # Loop through, computing expected value of pos scores, neg scores, and each one squared. 
    
    scoreFn = get(scoreFnName, mode="function")  # if not found, throws an error; if found, can now call it as scoreFn()
    
    totEPos = totENeg = totEPosSq = totENegSq = 0
    
    for (a in 0:n) {
        for (b in 0:(n-a)) {
            for (c in 0:(n-a-b)) {
                
                numWaysThisCombo = choose(n, a+b+c) * choose(a+b+c, a) * choose(b+c, b)
                d = (n - a - b - c)
                
                # note: we have to divide p10_neg by 2, because it evaluates to 2 * p_i * (1 - p_i), appropriate only when we don't 
                # distinguish events b and c. Likewise with p10_pos5.
                pComboForNeg = p11_neg(p_i)**a * (p10_neg(p_i)/2)**(b+c) * p00_neg(p_i)**d 
                
                pComboForPos = p11_pos5(p_i, trueT)**a * (p10_pos5(p_i, trueT)/2)**(b+c) * p00_pos5(p_i, trueT)**d 
                
                scoreForCombo = scoreFn(a_11=a, b_10 = b, c_01 = c, d_00 = d)
                
                newAmountEPos = numWaysThisCombo * pComboForPos * scoreForCombo
                newAmountENeg = numWaysThisCombo * pComboForNeg * scoreForCombo
                newAmountEPosSq = numWaysThisCombo * pComboForPos * scoreForCombo * scoreForCombo
                newAmountENegSq = numWaysThisCombo * pComboForNeg * scoreForCombo * scoreForCombo
                
                totEPos = totEPos + newAmountEPos
                totENeg = totENeg + newAmountENeg 
                totEPosSq = totEPosSq + newAmountEPosSq
                totENegSq = totENegSq + newAmountENegSq
            }
        }
    }
    varPos = totEPosSq - (totEPos**2)
    varNeg = totENegSq - (totENeg**2)
    
    return(AUC_template_theor(totEPos, varPos, totENeg, varNeg))
}

