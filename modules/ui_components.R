# UI Components Module - Dashboard Design
# Modern dashboard interface based on Example app

library(shinydashboard)
library(shinyjs)
library(DT)

# Dashboard CSS styling
dashboard_css <- function() {
  tags$head(
    tags$style(
      HTML("
        /* Basic styling */
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
          height: auto !important;
          min-height: 100vh;
          overflow-y: auto !important;
        }

        /* Fix scrolling issues */
        .tab-content {
          overflow-y: visible !important;
          height: auto !important;
          padding-bottom: 50px;
        }

        /* Prettier alerts */
        .alert {
          border-radius: 4px;
          margin-bottom: 20px;
        }

        .alert-info {
          background-color: #d1ecf1;
          border-color: #bee5eb;
          color: #0c5460;
        }

        .alert-success {
          background-color: #d4edda;
          border-color: #c3e6cb;
          color: #155724;
        }

        .alert-warning {
          background-color: #fff3cd;
          border-color: #ffeeba;
          color: #856404;
        }

        /* Progress indicator styling */
        .progress-indicator {
          padding: 10px;
          margin-top: 10px;
        }

        .progress-item {
          padding: 5px;
          margin-bottom: 5px;
          border-radius: 3px;
        }

        .progress-complete {
          background-color: #d4edda;
          color: #155724;
        }

        .progress-pending {
          background-color: #f8f9fa;
          color: #6c757d;
        }

        .progress-active {
          background-color: #cce5ff;
          color: #004085;
        }

        /* Method guide styling */
        .method-guide {
          background-color: #f8f9fa;
          border-left: 4px solid #007bff;
          padding: 10px 15px;
          margin-bottom: 15px;
        }

        .method-guide h4 {
          color: #007bff;
        }

        .method-guide ul {
          padding-left: 20px;
        }

        .method-recommendation {
          font-weight: bold;
          color: #155724;
          margin-top: 10px;
          padding: 5px;
          background-color: #d4edda;
          border-radius: 3px;
        }

        /* Custom box styling */
        .box.box-primary {
          border-top-color: #3498db;
        }

        .box.box-success {
          border-top-color: #27ae60;
        }

        .box.box-warning {
          border-top-color: #f39c12;
        }

        .box.box-info {
          border-top-color: #17a2b8;
        }

        /* Citation modal */
        .modal {
          display: none;
          position: fixed;
          z-index: 1000;
          left: 0;
          top: 0;
          width: 100%;
          height: 100%;
          background-color: rgba(0,0,0,0.8);
        }

        .modal-content {
          background-color: white;
          margin: 5% auto;
          padding: 0;
          border-radius: 15px;
          width: 80%;
          max-width: 600px;
          box-shadow: 0 20px 60px rgba(0,0,0,0.3);
          animation: modalFadeIn 0.3s;
        }

        .modal-header {
          background: linear-gradient(135deg, #3498db, #2980b9);
          color: white;
          padding: 25px;
          border-radius: 15px 15px 0 0;
          text-align: center;
        }

        .modal-body {
          padding: 30px;
          line-height: 1.6;
        }

        .modal-footer {
          padding: 20px 30px;
          background: #f8f9fa;
          border-radius: 0 0 15px 15px;
        }

        @keyframes modalFadeIn {
          from { opacity: 0; transform: translateY(-50px); }
          to { opacity: 1; transform: translateY(0); }
        }

        /* Checkbox group styling */
        .checkbox-group-custom {
          max-height: 200px;
          overflow-y: auto;
          border: 1px solid #ddd;
          border-radius: 5px;
          padding: 10px;
          background-color: white;
        }

        /* Download progress styling */
        .progress {
          background-color: #f5f5f5;
          border-radius: 4px;
          height: 20px;
          margin-bottom: 10px;
          overflow: hidden;
          box-shadow: inset 0 1px 2px rgba(0,0,0,.1);
        }

        .progress-bar {
          background-color: #337ab7;
          color: white;
          float: left;
          width: 0%;
          height: 100%;
          font-size: 12px;
          line-height: 20px;
          text-align: center;
          transition: width 0.6s ease;
        }
      "),

      # JavaScript for progress tracking
      tags$script("
        Shiny.addCustomMessageHandler('show_progress', function(message) {
          $('#download_progress').show();
          $('#progress_text').text(message);
          $('#progress_bar').css('width', '0%').text('0%');
        });

        Shiny.addCustomMessageHandler('update_progress', function(data) {
          $('#progress_text').text(data.message);
          $('#progress_bar').css('width', data.percent + '%').text(data.percent + '%');
        });

        Shiny.addCustomMessageHandler('hide_progress', function(message) {
          setTimeout(function() {
            $('#download_progress').hide();
          }, 2000);
        });
      ")
    )
  )
}
