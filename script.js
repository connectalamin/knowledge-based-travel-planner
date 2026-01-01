// ========================================
// VOYAGER - SMART TRAVEL PLANNER
// Enhanced Interactive UI
// ========================================

// --- Global State ---
let availableActivities = [];
let selectedActivities = ['beach', 'hiking'];
let currentResults = [];
let compareList = [];
let map = null;
let markers = [];
let currentView = 'cards';
let debounceTimer = null;

// --- Theme Management ---
function initTheme() {
    const savedTheme = localStorage.getItem('voyager-theme');
    const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
    const theme = savedTheme || (prefersDark ? 'dark' : 'light');
    document.documentElement.setAttribute('data-theme', theme);
    updateThemeIcon(theme);
}

function toggleTheme() {
    const current = document.documentElement.getAttribute('data-theme');
    const next = current === 'dark' ? 'light' : 'dark';
    document.documentElement.setAttribute('data-theme', next);
    localStorage.setItem('voyager-theme', next);
    updateThemeIcon(next);
    showToast(next === 'dark' ? 'Dark mode enabled' : 'Light mode enabled', 'info');
}

function updateThemeIcon(theme) {
    const btn = document.getElementById('themeToggle');
    if (btn) {
        btn.innerHTML = theme === 'dark' 
            ? '<i class="fa-solid fa-sun"></i>' 
            : '<i class="fa-solid fa-moon"></i>';
    }
}

// --- Toast Notifications ---
function showToast(message, type = 'info') {
    const container = document.getElementById('toastContainer');
    const toast = document.createElement('div');
    toast.className = `toast ${type}`;
    
    const icons = {
        success: 'fa-circle-check',
        error: 'fa-circle-exclamation',
        warning: 'fa-triangle-exclamation',
        info: 'fa-circle-info'
    };
    
    toast.innerHTML = `
        <i class="fa-solid ${icons[type]} toast-icon"></i>
        <span class="toast-message">${message}</span>
    `;
    
    container.appendChild(toast);
    
    setTimeout(() => {
        toast.classList.add('toast-out');
        setTimeout(() => toast.remove(), 300);
    }, 3000);
}

// --- Chip-based Activity Selection ---
function renderChips() {
    const container = document.getElementById('chipsContainer');
    container.innerHTML = selectedActivities.map(act => `
        <span class="chip" data-activity="${act}">
            ${act} <i class="fa-solid fa-xmark chip-remove" onclick="removeChip('${act}')"></i>
        </span>
    `).join('');
}

function addChip(activity) {
    if (!selectedActivities.includes(activity)) {
        selectedActivities.push(activity);
        renderChips();
        document.getElementById('activityInput').value = '';
        document.getElementById('suggestions').style.display = 'none';
        updateMatchCount();
    }
}

function removeChip(activity) {
    selectedActivities = selectedActivities.filter(a => a !== activity);
    renderChips();
    updateMatchCount();
}

// --- Safety Slider Value Display ---
function initSafetySlider() {
    const slider = document.getElementById('safety');
    const valueDisplay = document.getElementById('safetyValue');
    
    slider.addEventListener('input', () => {
        valueDisplay.textContent = slider.value;
        debouncedMatchCount();
    });
}

// --- Live Match Counter ---
function debouncedMatchCount() {
    clearTimeout(debounceTimer);
    debounceTimer = setTimeout(updateMatchCount, 300);
}

function updateMatchCount() {
    const payload = gatherInputs();
    const countText = document.getElementById('matchCountText');
    
    fetch("/recommend", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload)
    })
    .then(r => r.json())
    .then(data => {
        if (data.ok && data.results) {
            const count = data.results.length;
            countText.textContent = `${count} destination${count !== 1 ? 's' : ''} match your criteria`;
        } else {
            countText.textContent = 'No matches found';
        }
    })
    .catch(() => {
        countText.textContent = 'Unable to calculate';
    });
}

// --- Form Input Gathering ---
function gatherInputs() {
    return {
        budget: document.getElementById("budget").value,
        days: parseInt(document.getElementById("days").value) || 7,
        climate: document.getElementById("climate").value,
        visa: document.getElementById("visa").value,
        continent: document.getElementById("continent").value,
        month: parseInt(document.getElementById("month").value) || 0,
        activities: selectedActivities,
        safety: parseInt(document.getElementById("safety").value) || 3,
        food: document.getElementById("food").value
    };
}

// --- Reset Form ---
function resetForm() {
    document.getElementById("budget").value = "medium";
    document.getElementById("days").value = "7";
    document.getElementById("climate").value = "warm";
    document.getElementById("visa").value = "visa_free";
    document.getElementById("continent").value = "asia";
    document.getElementById("month").value = "0";
    document.getElementById("safety").value = "3";
    document.getElementById("safetyValue").textContent = "3";
    document.getElementById("food").value = "any";
    document.getElementById("activityInput").value = "";
    
    selectedActivities = ['beach', 'hiking'];
    renderChips();
    
    compareList = [];
    updateCompareButton();
    
    // Reset UI
    document.getElementById("initialState").style.display = "flex";
    document.getElementById("cardsView").style.display = "none";
    document.getElementById("mapView").style.display = "none";
    document.getElementById("viewTabs").style.display = "none";
    
    updateMatchCount();
    showToast('Form reset to defaults', 'info');
}

// --- Autocomplete Logic ---

// Fetch activities on load
document.addEventListener('DOMContentLoaded', () => {
    initTheme();
    initSafetySlider();
    renderChips();
    setupAutocomplete();
    setupFormListeners();
    
    fetch('/activities')
        .then(r => r.json())
        .then(data => {
            if (data.ok) {
                availableActivities = data.activities;
            }
        })
        .catch(console.error);
    
    // Initial match count
    setTimeout(updateMatchCount, 500);
});

function setupFormListeners() {
    // Add change listeners to all form inputs for live match count
    const inputs = ['budget', 'days', 'climate', 'visa', 'continent', 'month', 'food'];
    inputs.forEach(id => {
        const el = document.getElementById(id);
        if (el) {
            el.addEventListener('change', debouncedMatchCount);
        }
    });
}

function setupAutocomplete() {
    const input = document.getElementById('activityInput');
    const suggestionsBox = document.getElementById('suggestions');

    input.addEventListener('input', function() {
        const currentTerm = this.value.trim().toLowerCase();
        
        if (!currentTerm) {
            suggestionsBox.style.display = 'none';
            return;
        }

        // Filter activities - exclude already selected ones
        const matches = availableActivities.filter(act => 
            act.toLowerCase().includes(currentTerm) && 
            !selectedActivities.includes(act)
        );

        if (matches.length > 0) {
            const html = matches.map(match => {
                const regex = new RegExp(`(${currentTerm})`, 'gi');
                const highlighted = match.replace(regex, '<span class="match">$1</span>');
                return `<div class="suggestion-item" onclick="addChip('${match}')">${highlighted}</div>`;
            }).join('');
            
            suggestionsBox.innerHTML = html;
            suggestionsBox.style.display = 'block';
        } else {
            suggestionsBox.style.display = 'none';
        }
    });

    // Enter key to add first match
    input.addEventListener('keydown', function(e) {
        if (e.key === 'Enter') {
            e.preventDefault();
            const firstSuggestion = suggestionsBox.querySelector('.suggestion-item');
            if (firstSuggestion) {
                firstSuggestion.click();
            } else if (this.value.trim()) {
                // Add custom activity if no match
                addChip(this.value.trim().toLowerCase());
            }
        }
    });

    // Hide when clicking outside
    document.addEventListener('click', function(e) {
        if (e.target !== input && !suggestionsBox.contains(e.target)) {
            suggestionsBox.style.display = 'none';
        }
    });
}

function findDestinations() {
    // UI Handling
    const btn = document.getElementById("recommendBtn");
    const initialState = document.getElementById("initialState");
    const cardsView = document.getElementById("cardsView");
    const viewTabs = document.getElementById("viewTabs");
    
    // Show loading state
    btn.disabled = true;
    btn.innerHTML = '<i class="fa-solid fa-spinner fa-spin"></i> Processing...';
    initialState.style.display = "none";
    cardsView.style.display = "block";
    viewTabs.style.display = "flex";
    
    // Show skeleton loading
    cardsView.innerHTML = renderSkeletonCards();

    // Gather Inputs
    const payload = gatherInputs();

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

        currentResults = data.results;
        renderResults(data.results, payload);
        initMap(data.results);
        
        btn.disabled = false;
        btn.innerHTML = '<i class="fa-solid fa-sparkles"></i> Generate Plan';
        showToast(`Found ${data.results.length} destinations!`, 'success');
    })
    .catch(error => {
        console.error("Error:", error);
        renderError(error.message);
        btn.disabled = false;
        btn.innerHTML = '<i class="fa-solid fa-sparkles"></i> Generate Plan';
        showToast('Failed to fetch recommendations', 'error');
    });
}

// --- Skeleton Loading ---
function renderSkeletonCards() {
    let html = '<div class="skeleton-grid">';
    for (let i = 0; i < 6; i++) {
        html += `
            <div class="skeleton-card">
                <div class="skeleton-image"></div>
                <div class="skeleton-content">
                    <div class="skeleton-line title"></div>
                    <div class="skeleton-line subtitle"></div>
                    <div class="skeleton-line tags"></div>
                    <div class="skeleton-line"></div>
                </div>
            </div>
        `;
    }
    html += '</div>';
    return html;
}

function renderNoResults() {
    const container = document.getElementById("cardsView");
    container.innerHTML = `
        <div class="hero-text">
            <i class="fa-regular fa-face-frown-open"></i>
            <h2>No exact matches found.</h2>
            <p>Try adjusting your preferences or safety requirements.</p>
        </div>
    `;
}

function renderError(message) {
    const container = document.getElementById("cardsView");
    container.innerHTML = `
        <div class="hero-text">
            <i class="fa-solid fa-triangle-exclamation"></i>
            <h2>Error</h2>
            <p>${message}</p>
        </div>
    `;
}

function renderResults(results, inputs) {
    const container = document.getElementById("cardsView");
    
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

    // Get budget display
    function getBudgetDisplay(cost) {
        const budgetMap = {
            'cost_low': '$',
            'cost_medium': '$$',
            'cost_high': '$$$'
        };
        return budgetMap[cost] || '$$';
    }

    // Get visa display
    function getVisaDisplay(visa) {
        const visaMap = {
            'visa_free': 'Visa Free',
            'visa_on_arrival': 'Visa on Arrival',
            'visa_required': 'Visa Required',
            'schengen_required': 'Schengen Required'
        };
        return visaMap[visa] || visa;
    }

    // Get month names
    function getMonthNames(months) {
        const monthNames = ['', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
        return months.map(m => monthNames[m]).join(', ');
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
        const reasons = formatReasons(result.reasons);
        
        // Max score is 8. Calculate true percentage.
        const percentage = Math.round((result.score / 8) * 100);
        const confidence = Math.max(10, percentage);
        
        const capitalizedName = placeName.charAt(0).toUpperCase() + placeName.slice(1).replace(/_/g, ' ');
        const isCompared = compareList.includes(placeName);
        const imageQuery = placeName.replace(/_/g, ' ');

        html += `
            <div class="card" data-place="${placeName}" style="animation-delay: ${index * 0.1}s">
                <div class="compare-checkbox">
                    <input type="checkbox" id="compare-${placeName}" ${isCompared ? 'checked' : ''} 
                           onchange="toggleCompare('${placeName}')">
                    <label for="compare-${placeName}">
                        <i class="fa-solid ${isCompared ? 'fa-check' : 'fa-plus'}"></i>
                    </label>
                </div>
                <div class="card-image">
                    <img src="https://loremflickr.com/800/600/${imageQuery},landmark/all" 
                         alt="${capitalizedName}"
                         onerror="this.src='https://placehold.co/800x600?text=${encodeURIComponent(capitalizedName)}'">
                    <div class="match-badge">
                        <i class="fa-solid fa-check-circle"></i> ${confidence}% Match
                    </div>
                </div>
                <div class="card-content">
                    <div class="card-header">
                        <div>
                            <h3 class="card-title">${capitalizedName}</h3>
                            <div class="card-location"><i class="fa-solid fa-location-dot"></i> ${result.country}</div>
                        </div>
                    </div>
                    
                    <div class="tags">
                        ${reasons.map(r => `<span class="tag ${r.type}">${r.text}</span>`).join('')}
                        <span class="tag">Score: ${result.score.toFixed(1)}</span>
                    </div>

                    <div class="card-footer">
                        <div class="price">
                            ${getBudgetDisplay(result.cost)}
                            <span>Est. Cost</span>
                        </div>
                        <div class="safety-badge">
                            ${'★'.repeat(result.safety)}${'☆'.repeat(5 - result.safety)}
                        </div>
                    </div>
                </div>
                
                <!-- Expandable Details -->
                <div class="card-details">
                    <div class="detail-row">
                        <span class="detail-label"><i class="fa-solid fa-calendar-check"></i> Best Months</span>
                        <span class="detail-value">${getMonthNames(result.best_months)}</span>
                    </div>
                    <div class="detail-row">
                        <span class="detail-label"><i class="fa-solid fa-passport"></i> Visa</span>
                        <span class="detail-value">${getVisaDisplay(result.visa)}</span>
                    </div>
                    <div class="detail-row">
                        <span class="detail-label"><i class="fa-solid fa-utensils"></i> Food Scene</span>
                        <span class="detail-value">${result.food.replace(/_/g, ' ')}</span>
                    </div>
                    <div class="detail-row">
                        <span class="detail-label"><i class="fa-solid fa-list"></i> Activities</span>
                        <span class="detail-value">${result.activities.map(a => a.replace(/_/g, ' ')).join(', ')}</span>
                    </div>
                </div>
                
                <button class="expand-btn" onclick="toggleCardExpand(this)">
                    <span>Show Details</span>
                    <i class="fa-solid fa-chevron-down"></i>
                </button>
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

// --- Card Expansion ---
function toggleCardExpand(btn) {
    const card = btn.closest('.card');
    const isExpanded = card.classList.toggle('expanded');
    btn.querySelector('span').textContent = isExpanded ? 'Hide Details' : 'Show Details';
}

// ========================================
// MAP INTEGRATION (Leaflet.js)
// ========================================

function initMap(results) {
    const mapContainer = document.getElementById('map');
    
    // Destroy existing map if present
    if (map) {
        map.remove();
        map = null;
    }
    
    // Create new map with bounds to prevent repeating
    map = L.map('map', {
        center: [20, 0],
        zoom: 2,
        minZoom: 2,
        maxZoom: 18,
        maxBounds: [[-90, -180], [90, 180]],
        maxBoundsViscosity: 1.0
    });
    
    // Add tile layer (OpenStreetMap) with noWrap to prevent tiling
    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        attribution: '© OpenStreetMap contributors',
        maxZoom: 18,
        noWrap: true,
        bounds: [[-90, -180], [90, 180]]
    }).addTo(map);
    
    // Clear existing markers
    markers = [];
    
    // Add markers for each result
    results.forEach(result => {
        if (result.lat && result.lng) {
            const percentage = Math.round((result.score / 8) * 100);
            const capitalizedName = result.place.charAt(0).toUpperCase() + result.place.slice(1).replace(/_/g, ' ');
            
            // Custom marker icon
            const markerIcon = L.divIcon({
                className: 'custom-marker',
                html: `<div style="
                    background: var(--accent, #e36414);
                    color: white;
                    padding: 8px 12px;
                    border-radius: 20px;
                    font-weight: 600;
                    font-size: 12px;
                    box-shadow: 0 4px 12px rgba(0,0,0,0.3);
                    white-space: nowrap;
                    font-family: Inter, sans-serif;
                ">${capitalizedName}</div>`,
                iconSize: [100, 30],
                iconAnchor: [50, 15]
            });
            
            const marker = L.marker([result.lat, result.lng], { icon: markerIcon })
                .addTo(map)
                .bindPopup(`
                    <div class="map-popup">
                        <h4>${capitalizedName}</h4>
                        <p class="popup-country">${result.country}</p>
                        <p class="popup-score">${percentage}% Match</p>
                    </div>
                `);
            
            markers.push(marker);
        }
    });
    
    // Fit bounds to show all markers
    if (markers.length > 0) {
        const group = L.featureGroup(markers);
        map.fitBounds(group.getBounds().pad(0.2));
    }
}

// --- View Switching ---
function switchView(view) {
    currentView = view;
    
    // Update tab buttons
    document.querySelectorAll('.view-tab').forEach(tab => {
        tab.classList.toggle('active', tab.dataset.view === view);
    });
    
    // Show/hide views
    const cardsView = document.getElementById('cardsView');
    const mapView = document.getElementById('mapView');
    
    if (view === 'cards') {
        cardsView.style.display = 'block';
        mapView.style.display = 'none';
    } else {
        cardsView.style.display = 'none';
        mapView.style.display = 'block';
        
        // Invalidate map size when showing (fixes rendering issues)
        if (map) {
            setTimeout(() => map.invalidateSize(), 100);
        }
    }
}

// ========================================
// COMPARISON FEATURE
// ========================================

function toggleCompare(place) {
    const index = compareList.indexOf(place);
    
    if (index > -1) {
        // Remove from list
        compareList.splice(index, 1);
    } else {
        // Add to list (max 3)
        if (compareList.length >= 3) {
            showToast('Maximum 3 destinations can be compared', 'warning');
            // Uncheck the checkbox
            const checkbox = document.getElementById(`compare-${place}`);
            if (checkbox) checkbox.checked = false;
            return;
        }
        compareList.push(place);
    }
    
    // Update UI
    updateCompareButton();
    updateCompareCheckboxes();
}

function updateCompareButton() {
    const btn = document.getElementById('compareBtn');
    const count = document.getElementById('compareCount');
    
    if (compareList.length > 0) {
        btn.style.display = 'flex';
        count.textContent = compareList.length;
    } else {
        btn.style.display = 'none';
    }
}

function updateCompareCheckboxes() {
    document.querySelectorAll('.compare-checkbox input').forEach(checkbox => {
        const place = checkbox.id.replace('compare-', '');
        const isChecked = compareList.includes(place);
        checkbox.checked = isChecked;
        
        const label = checkbox.nextElementSibling;
        if (label) {
            label.innerHTML = `<i class="fa-solid ${isChecked ? 'fa-check' : 'fa-plus'}"></i>`;
        }
    });
}

function openComparisonModal() {
    if (compareList.length < 2) {
        showToast('Select at least 2 destinations to compare', 'warning');
        return;
    }
    
    const modal = document.getElementById('comparisonModal');
    const body = document.getElementById('comparisonBody');
    
    // Get data for selected destinations
    const compareData = compareList.map(place => 
        currentResults.find(r => r.place === place)
    ).filter(Boolean);
    
    // Build comparison table
    body.innerHTML = renderComparisonTable(compareData);
    modal.style.display = 'flex';
}

function closeComparisonModal() {
    document.getElementById('comparisonModal').style.display = 'none';
}

function renderComparisonTable(data) {
    const monthNames = ['', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
    
    const getVisaDisplay = (visa) => ({
        'visa_free': 'Visa Free',
        'visa_on_arrival': 'Visa on Arrival',
        'visa_required': 'Visa Required',
        'schengen_required': 'Schengen Required'
    })[visa] || visa;
    
    const getCostDisplay = (cost) => ({
        'cost_low': '$ Budget',
        'cost_medium': '$$ Moderate',
        'cost_high': '$$$ Luxury'
    })[cost] || cost;
    
    let html = `<table class="comparison-table">`;
    
    // Header row with destination names
    html += `<tr><th>Attribute</th>`;
    data.forEach(d => {
        const name = d.place.charAt(0).toUpperCase() + d.place.slice(1).replace(/_/g, ' ');
        html += `<th class="dest-header">${name}</th>`;
    });
    html += `</tr>`;
    
    // Country
    html += `<tr><td><i class="fa-solid fa-globe"></i> Country</td>`;
    data.forEach(d => html += `<td>${d.country}</td>`);
    html += `</tr>`;
    
    // Match Score
    html += `<tr><td><i class="fa-solid fa-star"></i> Match Score</td>`;
    data.forEach(d => {
        const pct = Math.round((d.score / 8) * 100);
        html += `<td class="score-cell">${pct}%</td>`;
    });
    html += `</tr>`;
    
    // Budget
    html += `<tr><td><i class="fa-solid fa-wallet"></i> Cost Level</td>`;
    data.forEach(d => html += `<td>${getCostDisplay(d.cost)}</td>`);
    html += `</tr>`;
    
    // Safety
    html += `<tr><td><i class="fa-solid fa-shield-halved"></i> Safety Rating</td>`;
    data.forEach(d => html += `<td>${'★'.repeat(d.safety)}${'☆'.repeat(5 - d.safety)}</td>`);
    html += `</tr>`;
    
    // Visa
    html += `<tr><td><i class="fa-solid fa-passport"></i> Visa</td>`;
    data.forEach(d => html += `<td>${getVisaDisplay(d.visa)}</td>`);
    html += `</tr>`;
    
    // Food
    html += `<tr><td><i class="fa-solid fa-utensils"></i> Food Scene</td>`;
    data.forEach(d => html += `<td>${d.food.replace(/_/g, ' ')}</td>`);
    html += `</tr>`;
    
    // Best Months
    html += `<tr><td><i class="fa-solid fa-calendar"></i> Best Months</td>`;
    data.forEach(d => {
        const months = d.best_months.map(m => monthNames[m]).join(', ');
        html += `<td>${months}</td>`;
    });
    html += `</tr>`;
    
    // Activities
    html += `<tr><td><i class="fa-solid fa-list"></i> Activities</td>`;
    data.forEach(d => {
        const tags = d.activities.map(a => 
            `<span class="mini-tag">${a.replace(/_/g, ' ')}</span>`
        ).join(' ');
        html += `<td class="activities-cell">${tags}</td>`;
    });
    html += `</tr>`;
    
    html += `</table>`;
    return html;
}

// Close modal on outside click
document.addEventListener('click', (e) => {
    const modal = document.getElementById('comparisonModal');
    if (e.target === modal) {
        closeComparisonModal();
    }
});

// Close modal on Escape key
document.addEventListener('keydown', (e) => {
    if (e.key === 'Escape') {
        closeComparisonModal();
    }
});
