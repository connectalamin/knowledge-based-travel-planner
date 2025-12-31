function resetForm() {
    document.getElementById("budget").value = "medium";
    document.getElementById("days").value = "7";
    document.getElementById("climate").value = "warm";
    document.getElementById("visa").value = "visa_free";
    document.getElementById("continent").value = "asia";
    document.getElementById("month").value = "0";
    document.getElementById("activities").value = "beach,hiking";
    document.getElementById("safety").value = "3";
    document.getElementById("food").value = "any";
    
    // Reset UI
    document.getElementById("initialState").style.display = "flex";
    document.getElementById("dynamicContent").style.display = "none";
}

// --- Autocomplete Logic ---
let availableActivities = [];

// Fetch activities on load
document.addEventListener('DOMContentLoaded', () => {
    fetch('/activities')
        .then(r => r.json())
        .then(data => {
            if (data.ok) {
                availableActivities = data.activities;
                setupAutocomplete();
            }
        })
        .catch(console.error);
});

function setupAutocomplete() {
    const input = document.getElementById('activities');
    const suggestionsBox = document.getElementById('suggestions');

    input.addEventListener('input', function() {
        const val = this.value;
        // Get the last term after the last comma
        const terms = val.split(',');
        const currentTerm = terms[terms.length - 1].trim().toLowerCase();
        
        if (!currentTerm) {
            suggestionsBox.style.display = 'none';
            return;
        }

        // Filter activities
        const matches = availableActivities.filter(act => 
            act.toLowerCase().includes(currentTerm) && 
            !terms.slice(0, -1).map(t => t.trim().toLowerCase()).includes(act) // Exclude already selected
        );

        if (matches.length > 0) {
            const html = matches.map(match => {
                // Highlight matching part
                const regex = new RegExp(`(${currentTerm})`, 'gi');
                const highlighted = match.replace(regex, '<span class="match">$1</span>');
                return `<div class="suggestion-item" onclick="selectActivity('${match}')">${highlighted}</div>`;
            }).join('');
            
            suggestionsBox.innerHTML = html;
            suggestionsBox.style.display = 'block';
        } else {
            suggestionsBox.style.display = 'none';
        }
    });

    // Hide when clicking outside
    document.addEventListener('click', function(e) {
        if (e.target !== input && e.target !== suggestionsBox) {
            suggestionsBox.style.display = 'none';
        }
    });
}

function selectActivity(activity) {
    const input = document.getElementById('activities');
    const terms = input.value.split(',');
    
    // Replace the last term with the selected activity
    terms.pop(); 
    terms.push(activity);
    
    // Join back and add a trailing comma and space for the next entry
    input.value = terms.map(t => t.trim()).join(', ') + ', ';
    
    document.getElementById('suggestions').style.display = 'none';
    input.focus();
}

function findDestinations() {
    // UI Handling
    const btn = document.getElementById("recommendBtn");
    const initialState = document.getElementById("initialState");
    const dynamicContent = document.getElementById("dynamicContent");
    
    // Show loading state
    btn.disabled = true;
    btn.innerHTML = '<i class="fa-solid fa-spinner fa-spin"></i> Processing...';
    initialState.style.display = "none";
    dynamicContent.style.display = "block";
    dynamicContent.innerHTML = `
        <div class="loading-spinner">
            <div class="spinner"></div>
            <p>Analyzing weather patterns, costs, and safety data...</p>
        </div>
    `;

    // Gather Inputs
    const payload = {
        budget: document.getElementById("budget").value,
        days: parseInt(document.getElementById("days").value) || 7,
        climate: document.getElementById("climate").value,
        visa: document.getElementById("visa").value,
        continent: document.getElementById("continent").value,
        month: parseInt(document.getElementById("month").value) || 0,
        activities: document.getElementById("activities").value
            .toLowerCase()
            .split(',')
            .map(s => s.trim())
            .filter(s => s),
        safety: parseInt(document.getElementById("safety").value) || 3,
        food: document.getElementById("food").value
    };

    // Call backend API
    fetch("/recommend", {
        method: "POST",
        headers: { 
            "Content-Type": "application/json",
            "Accept": "application/json"
        },
        body: JSON.stringify(payload)
    })
    .then(async r => {
        const responseText = await r.text();
        if (!r.ok) {
            try {
                const err = JSON.parse(responseText);
                throw new Error(err.details || err.error || `Server error: ${r.status}`);
            } catch (e) {
                throw new Error(`Server error: ${r.status} - ${responseText}`);
            }
        }
        return JSON.parse(responseText);
    })
    .then(data => {
        if (!data.ok || !data.results || data.results.length === 0) {
            renderNoResults();
            btn.disabled = false;
            btn.innerHTML = '<i class="fa-solid fa-sparkles"></i> Generate Plan';
            return;
        }

        renderResults(data.results, payload);
        btn.disabled = false;
        btn.innerHTML = '<i class="fa-solid fa-sparkles"></i> Generate Plan';
    })
    .catch(error => {
        console.error("Error:", error);
        renderError(error.message);
        btn.disabled = false;
        btn.innerHTML = '<i class="fa-solid fa-sparkles"></i> Generate Plan';
    });
}

function renderNoResults() {
    const container = document.getElementById("dynamicContent");
    container.innerHTML = `
        <div class="hero-text">
            <i class="fa-regular fa-face-frown-open"></i>
            <h2>No exact matches found.</h2>
            <p>Try adjusting your preferences or safety requirements.</p>
        </div>
    `;
}

function renderError(message) {
    const container = document.getElementById("dynamicContent");
    container.innerHTML = `
        <div class="hero-text">
            <i class="fa-solid fa-triangle-exclamation"></i>
            <h2>Error</h2>
            <p>${message}</p>
        </div>
    `;
}

function renderResults(results, inputs) {
    const container = document.getElementById("dynamicContent");
    
    // Map backend reasons to display format
    function formatReasons(reasons) {
        const reasonLabels = {
            budget_match: 'Budget Match',
            climate_match: 'Climate Match',
            visa_match: 'Visa Match',
            continent_match: 'Region Match',
            seasonal_match: 'Great Season',
            safety_match: 'Safety Match',
            food_match: 'Food Match',
            activity_match: 'Activity Match'
        };
        
        const formatted = [];
        for (const [key, value] of Object.entries(reasons)) {
            if (value > 0) {
                let label = reasonLabels[key] || key;
                // Special handling for partial matches
                if (key === 'activity_match') {
                    if (value < 1) {
                        label = `Activities: ${Math.round(value * 100)}%`;
                    } else {
                        label = 'All Activities Match';
                    }
                }
                if (key === 'seasonal_match') {
                    if (value === 1) label = 'Great Season';
                    else if (value === 0.5) label = 'Good Season (Shoulder)';
                }
                formatted.push({text: label, type: "highlight"});
            }
        }
        return formatted;
    }

    // Get destination metadata (for display purposes)
    function getDestinationInfo(placeName) {
        const placeMap = {
            'tokyo': { country: 'Japan', image: 'tokyo' },
            'bangkok': { country: 'Thailand', image: 'bangkok' },
            'osaka': { country: 'Japan', image: 'osaka' },
            'kathmandu': { country: 'Nepal', image: 'kathmandu' },
            'istanbul': { country: 'Turkey', image: 'istanbul' },
            'paris': { country: 'France', image: 'paris' },
            'dubai': { country: 'UAE', image: 'dubai' },
            'colombo': { country: 'Sri Lanka', image: 'colombo' },
            'kyoto': { country: 'Japan', image: 'kyoto' },
            'hanoi': { country: 'Vietnam', image: 'hanoi' },
            'london': { country: 'UK', image: 'london' },
            'new_york': { country: 'USA', image: 'new_york' },
            'sydney': { country: 'Australia', image: 'sydney' },
            'rome': { country: 'Italy', image: 'rome' },
            'cairo': { country: 'Egypt', image: 'cairo' },
            'cape_town': { country: 'South Africa', image: 'cape_town' },
            'rio': { country: 'Brazil', image: 'rio_de_janeiro' },
            'bali': { country: 'Indonesia', image: 'bali' },
            'amsterdam': { country: 'Netherlands', image: 'amsterdam' },
            'barcelona': { country: 'Spain', image: 'barcelona' }
        };
        return placeMap[placeName.toLowerCase()] || { country: placeName, image: 'travel' };
    }

    // Get budget display
    function getBudgetDisplay(budget) {
        const budgetMap = {
            'low': '$',
            'medium': '$$',
            'high': '$$$'
        };
        return budgetMap[budget] || '$$';
    }

    let html = `
        <div class="results-header">
            <div>
                <h2>Top Recommendations</h2>
                <p class="results-count">Found ${results.length} destination${results.length > 1 ? 's' : ''} matching your preferences</p>
            </div>
        </div>
        <div class="grid">
    `;

    results.forEach((result, index) => {
        const placeName = result.place;
        const info = getDestinationInfo(placeName);
        const reasons = formatReasons(result.reasons);
        
        // Max score is 8. Calculate true percentage.
        const percentage = Math.round((result.score / 8) * 100);
        const confidence = Math.max(10, percentage); // Minimum 10% to avoid 0%
        
        const capitalizedName = placeName.charAt(0).toUpperCase() + placeName.slice(1);
        
        // Determine budget from score or use default
        let budgetLevel = 'medium';
        if (result.reasons.budget_match === 1) {
            // Try to infer from place name or use default
            budgetLevel = inputs.budget || 'medium';
        }

        html += `
            <div class="card" style="animation-delay: ${index * 0.1}s">
                <div class="card-image">
                    <img src="https://loremflickr.com/800/600/${info.image},landmark/all" 
                         alt="${capitalizedName}"
                         onerror="this.src='https://placehold.co/800x600?text=${capitalizedName}'">
                    <div class="match-badge">
                        <i class="fa-solid fa-check-circle"></i> ${confidence}% Match
                    </div>
                </div>
                <div class="card-content">
                    <div class="card-header">
                        <div>
                            <h3 class="card-title">${capitalizedName}</h3>
                            <div class="card-location"><i class="fa-solid fa-location-dot"></i> ${info.country}</div>
                        </div>
                    </div>
                    
                    <div class="tags">
                        ${reasons.map(r => `<span class="tag ${r.type}">${r.text}</span>`).join('')}
                        <span class="tag">Score: ${result.score.toFixed(1)}</span>
                    </div>

                    <div class="card-footer">
                        <div class="price">
                            ${getBudgetDisplay(budgetLevel)}
                            <span>Est. Cost</span>
                        </div>
                    </div>
                </div>
            </div>
        `;
    });

    html += `</div>`;
    container.innerHTML = html;
    
    // Scroll to results on mobile
    if(window.innerWidth < 900) {
        container.scrollIntoView({ behavior: 'smooth' });
    }
}
