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
                const label = reasonLabels[key] || key;
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
            'hanoi': { country: 'Vietnam', image: 'hanoi' }
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
        const confidence = Math.min(99, Math.max(60, Math.round(result.score * 10)));
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
                    <img src="https://source.unsplash.com/800x600/?${info.image},travel,landmark" 
                         alt="${capitalizedName}"
                         onerror="this.src='https://images.unsplash.com/photo-1469854523086-cc02fe5d8800?auto=format&fit=crop&w=800&q=80'">
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
                        <button class="btn-primary" style="width: auto; padding: 8px 16px; font-size: 0.85rem; margin-top:0;" onclick="alert('Full itinerary feature coming soon!')">
                            View Plan
                        </button>
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
