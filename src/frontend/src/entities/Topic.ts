import { apiClient } from '~/shared/apiClient'

export interface Topic {
  code: string
  title: string
  lessons: string[]
  traits: string[]
  description: string
}

export const TopicsApi = {
  async getAll() {
    const { data } = await apiClient<Topic[]>('/Topics/GetTopics')

    return data
  },
}
