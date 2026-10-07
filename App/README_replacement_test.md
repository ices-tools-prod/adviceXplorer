# SID vs SAG Webservice Replacement Investigation

## Summary
This investigation tests whether the SID webservice can be replaced by the SAG latestStocks webservice for the adviceXplorer application.

## Key Findings

### ❌ Direct Replacement is NOT Feasible

**Critical Issue: Missing EcoRegion Data**
- The `EcoRegion` field from SID is essential for the application's geographic functionality
- SAG webservices do not provide ecoregion information
- The `separate_ecoregions()` function depends on this field to create one row per ecoregion

**Missing Fields Analysis:**
- ✗ `EcoRegion`: Not available in any SAG endpoint
- ✗ `DataCategory`: Not available in SAG
- ✗ `AssessmentFrequency`: Not available in SAG  
- ✗ `YearOfNextAssessment`: Not available in SAG
- ✗ `SpeciesCommonName`: May be derivable from description but not directly available

**Available Field Mappings:**
- ✓ `StockKeyLabel` → `StockKeyLabel`
- ✓ `StockKeyDescription` → `StockDescription` 
- ✓ `AssessmentKey` → `AssessmentKey`
- ✓ `YearOfLastAssessment` → `AssessmentYear`

## Recommended Approaches

### 1. ✅ Hybrid Approach (Recommended)
Keep SID for metadata and use SAG for assessment data:

```r
# Use SID for: EcoRegion, DataCategory, Assessment planning
sid_metadata <- download_SID(year) 

# Use SAG for: Latest assessment information, detailed data
sag_assessments <- getListStocks(year = year)

# Merge for optimal data combination
combined_data <- merge(sid_metadata, sag_assessments, by = "StockKeyLabel")
```

### 2. 🔄 Enhanced Integration
- Request ICES to add ecoregion data to SAG endpoints
- Create and maintain a stock-to-ecoregion lookup table
- Accept functional limitations for some metadata

### 3. 🚫 Direct Replacement
Not recommended due to loss of critical functionality.

## Test Implementation

A test function `test_sag_replacement()` has been created in `test_sag_replacement.r` to:
- Compare data availability between SID and SAG endpoints
- Analyze field compatibility
- Provide automated feasibility assessment
- Demonstrate hybrid approach implementation

## Usage

```r
# Source the test functions
source("App/test_sag_replacement.r")

# Run the compatibility test
results <- test_sag_replacement(2024)

# Try the hybrid approach
combined_data <- get_combined_stock_data(2024)
```

## Conclusion

**The SID webservice cannot be directly replaced by SAG latestStocks** without significant loss of functionality. The recommended approach is to maintain the current SID integration while optimizing the use of SAG for assessment-specific data.

The EcoRegion field is the primary blocker, as it's essential for the application's geographic filtering and mapping capabilities that users expect from adviceXplorer.