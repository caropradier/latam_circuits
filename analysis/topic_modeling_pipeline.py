import pickle
import pandas as pd
import numpy as np
from sklearn.decomposition import PCA
from sklearn.feature_extraction.text import CountVectorizer
from umap import UMAP
from sentence_transformers import SentenceTransformer
from hdbscan import HDBSCAN
from bertopic import BERTopic
import os
import logging
import sys
from datetime import datetime
from threadpoolctl import threadpool_limits
from nltk.corpus import stopwords
from bertopic.vectorizers import ClassTfidfTransformer
from bertopic.representation import KeyBERTInspired



class TopicModelingPipeline:
    def __init__(self, data_path='../data/sample_text.csv', embeddings_path='../embeddings.p'):
        self.data_path = data_path
        self.embeddings_path = embeddings_path
        self.df = None
        self.embeddings = None
        self.texts = None
        self.levels = None
        self.pca_embeddings = None
        self.umap_model = None
        self.vectorizer_model = None
        self.hdbscan_model = None
        self.topic_model = None
        self.ctfidf_model = None
        self.representation_model = None

        # Initialize logger
        self.logger = logging.getLogger(__name__)
        self.logger.setLevel(logging.INFO)
        formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')

        # Log to console
        ch = logging.StreamHandler()
        ch.setLevel(logging.INFO)
        ch.setFormatter(formatter)
        self.logger.addHandler(ch)

        # Log to file
        self.log_folder = None
        self.create_output_folder()  # Create output folder
        log_file = os.path.join(self.log_folder, 'pipeline.log')  # Define log file path
        fh = logging.FileHandler(log_file)  # File handler for logging to file
        fh.setLevel(logging.INFO)
        fh.setFormatter(formatter)
        self.logger.addHandler(fh)  # Add file handler to logger

    def load_data(self):
        try:
            self.logger.info("Loading data from: {}".format(self.data_path))
            self.df = pd.read_csv(self.data_path, delimiter='\t')
            self.texts = self.df.text.values
            self.levels = self.df.level1_codes.values
        except Exception as e:
            self.logger.error("Error loading data: {}".format(e))

    def preprocess_embeddings(self):
        try:
            self.logger.info("Preprocessing embeddings")
            with threadpool_limits(limits=1):
                self.pca_embeddings = self.rescale(PCA(n_components=5).fit_transform(self.embeddings))
        except Exception as e:
            self.logger.error("Error preprocessing embeddings: {}".format(e))

    def rescale(self, x, inplace=False):
        try:
            self.logger.info("Rescaling embeddings")
            if not inplace:
                x = np.array(x, copy=True)
            x /= np.std(x[:, 0]) * 10000
            return x
        except Exception as e:
            self.logger.error("Error rescaling embeddings: {}".format(e))

    def initialize_umap_model(self):
        try:
            self.logger.info("Initializing UMAP model")
            self.umap_model = UMAP(
                n_neighbors=15,
                n_components=5,
                min_dist=0.0,
                metric="cosine",
                init=self.pca_embeddings,
                random_state=1234,
                n_jobs=10,
                low_memory=False
            )
        except Exception as e:
            self.logger.error("Error initializing UMAP model: {}".format(e))

    def initialize_vectorizer_model(self):
        try:
            self.logger.info("Initializing CountVectorizer model")
            self.vectorizer_model = CountVectorizer(ngram_range=(1,1), 
                                                    stop_words=(stopwords.words(['english','spanish','portuguese']) + ['ttttusepackage','amssymb', 'amsmath', 'mathrsfs', 'amsbsy',
                   'amsfonts', 'oddsidemargin', 'wasysym', '69pt', 'upgreek', '12pt',
                   'ttttbegin', 'ttttsetlength', 'document','mso', 'font','tstyle', '4pt','0pt',
                   'style','0cm','margin','calibri','0001pt','qformat','msonormaltable','pagination','rowband', 'size',
                   '0400', 'colband','bottom']), 
                   max_df=1.0, min_df=0.001)
        except Exception as e:
            self.logger.error("Error initializing CountVectorizer model: {}".format(e))

    def initialize_hdbscan_model(self):
        try:
            self.logger.info("Initializing HDBSCAN model")
            self.hdbscan_model = HDBSCAN(min_cluster_size=100,min_samples=1, 
                                         metric='euclidean',cluster_selection_epsilon=0.05, 
                                         cluster_selection_method='leaf', prediction_data=True,
                                         core_dist_n_jobs=10,memory='tmp/')
        except Exception as e:
            self.logger.error("Error initializing HDBSCAN model: {}".format(e))

    def initialize_ctfidf_model(self):
        try:
            self.logger.info("Initializing ClassTfidfTransformer model")
            self.ctfidf_model = ClassTfidfTransformer()
        except Exception as e:
            self.logger.error("Error initializing ClassTfidfTransformer model: {}".format(e))

    def initialize_representation_model(self):
        try:
            self.logger.info("Initializing KeyBERTInspired model")
            self.representation_model = KeyBERTInspired()
        except Exception as e:
            self.logger.error("Error initializing KeyBERTInspired model: {}".format(e))

    def encode_and_save_embeddings(self):
        try:
            self.logger.info("Encoding and saving embeddings")
            if not os.path.exists(self.embeddings_path):
                self.embeddings_path = os.path.join(self.log_folder, 'embeddings.p')
                sentence_model = SentenceTransformer("paraphrase-multilingual-MiniLM-L12-v2")
                pool=sentence_model.start_multi_process_pool()
                embeddings = sentence_model.encode_multi_process(self.texts,pool=pool)
                #embeddings = sentence_model.encode(self.texts, show_progress_bar=False)
                self.save_embeddings(embeddings)
                sentence_model.stop_multi_process_pool(pool)
                self.sentence_model = sentence_model
            else:
                self.sentence_model = SentenceTransformer("paraphrase-multilingual-MiniLM-L12-v2")
        except Exception as e:
            self.logger.error("Error encoding and saving embeddings: {}".format(e))
        
    def save_embeddings(self, embeddings):
        try:
            self.logger.info("Saving embeddings to: {}".format(self.embeddings_path))
            with open(self.embeddings_path, 'wb') as handle:
                pickle.dump(embeddings, handle, protocol=pickle.HIGHEST_PROTOCOL)
        except Exception as e:
            self.logger.error("Error saving embeddings: {}".format(e))

    def load_embeddings(self):
        try:
            self.logger.info("Loading embeddings from: {}".format(self.embeddings_path))
            with open(self.embeddings_path, 'rb') as handle:
                self.embeddings = pickle.load(handle)
        except Exception as e:
            self.logger.error("Error loading embeddings: {}".format(e))

    def fit_topic_model(self):
        try:
            self.logger.info("Fitting BERTopic model")
            self.topic_model = BERTopic(verbose=True,
                                        embedding_model=self.sentence_model, 
                                        low_memory=True, 
                                        calculate_probabilities=False, 
                                        vectorizer_model=self.vectorizer_model, 
                                        hdbscan_model=self.hdbscan_model,
                                        umap_model=self.umap_model,
                                        ctfidf_model=self.ctfidf_model, 
                                        representation_model=self.representation_model)
            topics, probabilities = self.topic_model.fit_transform(self.texts, self.embeddings,y=self.levels)
        except Exception as e:
            self.logger.error("Error fitting BERTopic model: {}".format(e))

    def save_topic_model(self, save_path):
        try:
            self.logger.info("Saving BERTopic model to: {}".format(save_path))
            sentence_model = "paraphrase-multilingual-MiniLM-L12-v2"
            self.topic_model.save(save_path,serialization='safetensors',save_ctfidf=True,save_embedding_model=sentence_model)
        except Exception as e:
            self.logger.error("Error saving BERTopic model: {}".format(e))
    
    def save_topic_info(self, file_path):
        try:
            self.logger.info("Saving topic info to file: {}".format(file_path))
            topic_info = self.get_topic_info()
            topic_info.to_csv(file_path, index=False)
        except Exception as e:
            self.logger.error("Error saving topic info: {}".format(e))

    def save_document_topics(self, file_path):
        try:
            self.logger.info("Saving document topics to file: {}".format(file_path))
            document_info = self.get_document_info()
            doc_topic = self.df['Pub_ID']
            doc_topic = pd.concat([doc_topic, document_info[['Topic']]], axis=1)
            doc_topic.to_csv(file_path, index=False)
        except Exception as e:
            self.logger.error("Error saving document topics: {}".format(e))

    def create_output_folder(self):
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        self.log_folder = f"../job_outputs/bertopic_output_{timestamp}"
        os.makedirs(self.log_folder)
        self.logger.info(f"Output folder created: {self.log_folder}")

    def run_pipeline(self):
        try:
            model_save_path = os.path.join(self.log_folder, 'topic_model')
            topic_info_file = os.path.join(self.log_folder, 'topic_info.csv')
            document_topics_file = os.path.join(self.log_folder, 'document_topics.csv')

            self.logger.info("Starting pipeline")
            self.load_data()
            self.encode_and_save_embeddings()
            self.load_embeddings()
            self.preprocess_embeddings()
            self.initialize_vectorizer_model()
            self.initialize_hdbscan_model()
            self.initialize_umap_model()
            self.initialize_ctfidf_model()
            self.initialize_representation_model ()
            self.fit_topic_model()
            self.save_topic_model(model_save_path)
            self.save_topic_info(topic_info_file)
            self.save_document_topics(document_topics_file)
            self.logger.info("Pipeline completed successfully. Model saved at: {}".format(model_save_path))
        except Exception as e:
            self.logger.error("Pipeline failed with error: {}".format(e))

    def get_document_info(self):
        try:
            self.logger.info("Getting document info")
            return self.topic_model.get_document_info(self.texts)
        except Exception as e:
            self.logger.error("Error getting document info: {}".format(e))

    def get_topic_info(self):
        try:
            self.logger.info("Getting topic info")
            return self.topic_model.get_topic_info()
        except Exception as e:
            self.logger.error("Error getting topic info: {}".format(e))

if __name__ == "__main__":
    # Set up logging
    logging.basicConfig(level=logging.INFO)

    if len(sys.argv) != 2:#3:
        print("Usage: python topic_modeling_pipeline.py <data_path> <embeddings_path>")
        sys.exit(1)

    data_path = sys.argv[1]
    #embeddings_path= sys.argv[2]
    pipeline = TopicModelingPipeline(data_path=data_path)#, embeddings_path=embeddings_path)
    pipeline.run_pipeline()